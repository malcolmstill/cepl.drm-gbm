(in-package :cepl.drm-gbm)

(defvar *initd* nil)
(defvar *drm-gbm* nil)

(defclass drm-gbm ()
  ((fd :accessor fd :initarg :fd :initform nil)
   ;; DRM
   (display-config :accessor display-config :initarg :display-config :initform nil)
   (mode-info :accessor mode-info :initarg :mode-info :initform nil)
   (connector-id :accessor connector-id :initarg :connector-id :initform nil)
   (crtc :accessor crtc :initarg :crtc :initform nil)
   (width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (previous-fb :accessor previous-fb :initarg :previous-fb :initform nil)
   (page-flip-scheduled? :accessor page-flip-scheduled? :initarg :page-flip-scheduled? :initform nil)
   ;; GBM
   (gbm-device :accessor gbm-device :initarg :gbm-device :initform nil)
   (gbm-surface :accessor gbm-surface :initarg :gbm-surface :initform nil)
   (previous-bo :accessor previous-bo :initarg :previous-bo :initform nil)
   ;; EGL
   (egl-display :accessor egl-display :initarg :egl-display :initform nil)
   (egl-surface :accessor egl-surface :initarg :egl-surface :initform nil)
   (egl-context :accessor egl-context :initarg :egl-context :initform nil)))

(defun drm-gbm-init ()
  (unless *initd*
    (setf *drm-gbm* (make-instance 'drm-gbm))
    (setf *initd* t)))

(defun drm-gbm-make-surface
    (width height title fullscreen
     no-frame alpha-size depth-size stencil-size
     red-size green-size blue-size buffer-size
     double-buffer hidden resizable)
  "Initializes the backend and returns a list containing: (context window)"
  (let* ((fd (nix:open "/dev/dri/card0" nix:o-rdwr))
	 (display-config (drm:find-display-configuration fd))
	 (mode (drm:mode-info display-config))
	 (connector-id (drm:connector-id display-config))
	 (crtc (drm:crtc display-config))
	 (gbm-device (gbm:create-device fd))
	 (egl-display (egl:get-display gbm-device)))
    (multiple-value-bind (major minor) (egl:initialize egl-display)
      (format t "EGL version: ~d.~d~%" major minor))
    (egl:bind-api :opengl-api)
    (format t "OpenGL API bound~%")
    (let* ((egl-config (first (egl:choose-config egl-display 1
						 :red-size 8
						 :green-size 8
						 :blue-size 8
						 :none)))
	   (egl-context (egl:create-context egl-display
					    egl-config
					    (null-pointer) ; EGL_NO_CONTEXT
					    :context-major-version 3
					    :context-minor-version 1
					    :none))
	   (mode-width (foreign-slot-value mode '(:struct drm:mode-mode-info) 'drm:hdisplay))
	   (mode-height (foreign-slot-value mode '(:struct drm:mode-mode-info) 'drm:vdisplay))
	   (gbm-surface (gbm:surface-create gbm-device
					    mode-width
					    mode-height
					    0 ; GBM_BO_FORMAT_XRGB8888
					    5 ; SCANOUT | RENDERING
					    ))
	   (egl-surface (egl:create-window-surface egl-display
						   egl-config
						   gbm-surface
						   (null-pointer))))
      (format t "Mode resolution: ~dx~d~%" mode-width mode-height)
      (setf (fd *drm-gbm*) fd)
      (setf (mode-info *drm-gbm*) mode)
      (setf (crtc *drm-gbm*) crtc)
      (setf (connector-id *drm-gbm*) connector-id)
      (setf (width *drm-gbm*) mode-width)
      (setf (height *drm-gbm*) mode-height)
      (when (= (egl:make-current egl-display egl-surface egl-surface egl-context) 0)
	(error "Failed to make current with code ~d" (egl::get-error)))
      (setf (egl-display *drm-gbm*) egl-display)
      (setf (egl-surface *drm-gbm*) egl-surface)
      (setf (egl-context *drm-gbm*) egl-context)
      (setf (gbm-device *drm-gbm*) gbm-device)
      (setf (gbm-surface *drm-gbm*) gbm-surface)
      (setf (display-config *drm-gbm*) display-config)
      (list egl-context *drm-gbm*))))
;; The return value (list egl-context *drm-gbm*) is what
;; the CEPL host api passes in as surface

(defun drm-gbm-destroy-surface (surface)
  (format t "destroy-surface does nothing"))

(defun drm-gbm-make-context
    (surface version double-buffer
     alpha-size depth-size stencil-size buffer-size
     red-size green-size blue-size)
  (format t "make-context does nothing"))

(defun drm-gbm-shutdown ()
  (let ((drm-gbm *drm-gbm*))
    (format t "Destroying backend~%")
    (let ((fd (fd drm-gbm))
	  (crtc (crtc drm-gbm))
	  (previous-bo (previous-bo drm-gbm))
	  (gbm-surface (gbm-surface drm-gbm))
	  (egl-display (egl-display drm-gbm)))
    
    ;; Restore previous crtc
    (when crtc
      (with-foreign-objects ((connector-id :uint32))
	(setf (mem-aref connector-id :uint32) (connector-id drm-gbm))
	(drm:mode-set-crtc fd
			   (foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:crtc-id)
			   (foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:buffer-id)
			   (foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:x)
			   (foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:y)
			   connector-id
			   1
			   (incf-pointer crtc (foreign-slot-offset '(:struct drm:mode-crtc) 'drm:mode)))
	(format t "Reset CRTC~%")
	
	;;(drm:mode-free-crtc crtc)
	;;(format t "CRTC freed~%")
	))
    
    
    ;; Remove FB
    (when previous-bo
      (drm:mode-remove-framebuffer fd (previous-fb drm-gbm))
      (gbm:surface-release-buffer gbm-surface previous-bo))

    (format t "Frame buffer removed~%")
    
    (when (egl-surface drm-gbm)
      (egl:destroy-surface egl-display (egl-surface drm-gbm)))
    (when gbm-surface (gbm:surface-destroy gbm-surface))
    (when (egl-context drm-gbm)
      (egl:destroy-context egl-display (egl-context drm-gbm)))
    (when egl-display (egl:terminate egl-display))
    (when (gbm-device drm-gbm)
      (gbm:device-destroy (gbm-device drm-gbm)))
    (when fd (nix:close fd)))))

;; In this case surface is a list of egl-context drm-gbm
(defun drm-gbm-swap (surface)
  (format t "DRM-GBM-SWAP: ~A~%" surface)
  (let ((drm-gbm (second surface)))
    (when (not (page-flip-scheduled? drm-gbm))
      (egl:swap-buffers (egl-display drm-gbm) (egl-surface drm-gbm))
      (page-flip drm-gbm))
    ;;(set-mode drm-gbm)
    ))

(defun set-mode (drm-gbm)
  (let* ((new-bo (gbm:surface-lock-front-buffer (gbm-surface drm-gbm)))
	 (handle (gbm:bo-get-handle new-bo))
	 (pitch (gbm:bo-get-stride new-bo))
	 (fd (fd drm-gbm)))
    (with-foreign-objects ((fb :uint32) (connector-id :uint32))
      (setf (mem-aref connector-id :uint32) (connector-id drm-gbm))
      (drm:mode-add-framebuffer fd
				(width drm-gbm) (height drm-gbm)
				24 32 pitch handle fb)
      (drm:mode-set-crtc fd (foreign-slot-value (crtc drm-gbm) '(:struct drm:mode-crtc) 'drm:crtc-id) (mem-aref fb :uint32) 0 0 connector-id
			 1 (mode-info drm-gbm))
      (when (previous-bo drm-gbm)
	(drm:mode-remove-framebuffer fd (previous-fb drm-gbm))
	(gbm:surface-release-buffer (gbm-surface drm-gbm) (previous-bo drm-gbm)))
      (setf (previous-bo drm-gbm) new-bo)
      (setf (previous-fb drm-gbm) (mem-aref fb :uint32)))))

(defun page-flip (drm-gbm)
  (let* ((new-bo (gbm:surface-lock-front-buffer (gbm-surface drm-gbm)))
	 (handle (gbm:bo-get-handle new-bo))
	 (pitch (gbm:bo-get-stride new-bo))
	 (fd (fd drm-gbm)))
    (with-foreign-objects ((fb :uint32) (connector-id :uint32))
      (setf (mem-aref connector-id :uint32) (connector-id drm-gbm))
      (drm:mode-add-framebuffer fd (width drm-gbm) (height drm-gbm) 24 32 pitch handle fb)
      ;;(drm:mode-set-crtc fd (foreign-slot-value (crtc drm-gbm) '(:struct drm:mode-crtc) 'drm:crtc-id) (mem-aref fb :uint32) 0 0 connector-id 1 (mode-info drm-gbm))
      (let ((crtc-id (foreign-slot-value (crtc drm-gbm) '(:struct drm:mode-crtc) 'drm:crtc-id)))
	(drm:mode-page-flip fd crtc-id (mem-aref fb :uint32) 1 (null-pointer)))
      (setf (page-flip-scheduled? drm-gbm) t)
      (when (previous-bo drm-gbm)
	(drm:mode-remove-framebuffer fd (previous-fb drm-gbm))
	(gbm:surface-release-buffer (gbm-surface drm-gbm) (previous-bo drm-gbm)))
      (setf (previous-bo drm-gbm) new-bo)
      (setf (previous-fb drm-gbm) (mem-aref fb :uint32)))))

(defmethod set-primary-thread-and-run (func &rest args)
  ;;(sdl2:make-this-thread-main (lambda () (apply func args)))
  )

;;----------------------------------------------------------------------
;; event stub

(defun collect-drm-gbm-events (drm-gbm &optional tpref)
  (declare (ignore drm-gbm tpref))
  ;;(%case-events (event)
  ;;  (:quit () (cepl.host:shutdown)))
  )

;;----------------------------------------------------------------------
;; window size

(defun drm-gbm-win-size (drm-gbm)
  (list (width drm-gbm) (height drm-gbm)))

;;----------------------------------------------------------------------
;; tell cepl what to use

(defun make-shared-drm-gbm-context (current-gl-context surface version double-buffer
                                    alpha-size depth-size stencil-size buffer-size
                                    red-size green-size blue-size)
  )


(defclass drm-gbm-api (cepl.host:api-2)
  (;;
   (supports-multiple-contexts-p :initform nil)
   ;;
   (supports-multiple-surfaces-p :initform nil)
   ;;
   (init-function :initform #'drm-gbm-init)
   ;;
   (shutdown-function :initform #'drm-gbm-shutdown)
   ;;
   (make-surface-function :initform #'drm-gbm-make-surface)
   ;;
   (destroy-surface-function :initform #'drm-gbm-destroy-surface)
   ;;
   (make-context-function :initform #'drm-gbm-make-context)
   ;;
   (step-function :initform #'collect-drm-gbm-events)
   ;;
   (register-event-callback-function :initform (lambda (fn)
						 ))
   ;;
   (swap-function :initform #'drm-gbm-swap)
   ;;
   (surface-size-function :initform (lambda (surface)
				      (list (width *drm-gbm*)
					    (height *drm-gbm*))))
   ;;
   (make-context-current-function :initform (lambda (gl-context surface)))
   ;;
   (set-surface-size-function :initform (lambda (surface width height)))
   ;;
   (surface-fullscreen-p-function :initform (lambda (surface) t))
   ;;
   (set-surface-fullscreen-function :initform (lambda (surface)))
   ;;
   (surface-title-function :initform (lambda (surface)
				       "Linux DRM/KMS"))
   ;;
   (set-surface-title-function :initform (lambda (surface title)
					   ))
   ;;
   (make-gl-context-shared-with-current-context-function
    :initform #'make-shared-drm-gbm-context)))

(register-host 'drm-gbm-api)
