;;;; cepl.glfw.lisp

(in-package #:cepl.glfw)

(declaim (optimize (debug 3)))
;;======================================================================
;; api v1

(defgeneric glfw-init (&rest init-flags)
  (:method (&rest init-flags)
    (declare (ignore init-flags))

    (glfw:initialize)))

(defun init-glfw-low-level (&rest glfw-init-flags)
  (declare (ignore glfw-init-flags))

  (glfw:initialize))

;;----------------------------------------------------------------------

(defun glfw-shutdown ()
  (low-level-quit))

(defun low-level-quit ()
  (glfw:terminate))

;;----------------------------------------------------------------------

(let ((listeners nil))
  ;;
  (defun glfw-register-listener (func)
    (unless (find func listeners)
      (push func listeners)))
  ;;
  (defun glfw-step-v1 (surface)
    (declare (ignore surface))
    ;; (break)
    (glfw:poll-events)))

;;----------------------------------------------------------------------

(defun glfw-swap (handle)
  (glfw:swap-buffers handle))

;;----------------------------------------------------------------------

(defun make-glfw-context (surface version double-buffer
                          alpha-size depth-size stencil-size buffer-size
                          red-size green-size blue-size)

  (declare (ignore version double-buffer
                   alpha-size depth-size stencil-size buffer-size
                   red-size green-size blue-size))
  surface)

(defvar *core-context* t)

(defun glfw-make-current (context surface)
  (glfw:make-context-current (or context surface)))

;;----------------------------------------------------------------------

(defun make-glfw-surface (width height title fullscreen
                          no-frame alpha-size depth-size stencil-size
                          red-size green-size blue-size buffer-size
                          double-buffer hidden resizable)
  (declare (ignore fullscreen buffer-size))
  (labels
      ((create-window (major minor)
         (%glfw:window-hint #X00021010 (if double-buffer 1 0))

         (glfw:create-window :width width
                             :height height
                             :title title
                             :resizable resizable
                             :visible (not hidden)
                             :decorated (not no-frame)

                             :red-bits red-size
                             :green-bits green-size
                             :blue-bits blue-size
                             :depth-bits depth-size
                             :stencil-bits stencil-size
                             :alpha-bits alpha-size

                             :opengl-profile (if *core-context*
                                                 :opengl-core-profile
                                                 :opengl-compat-profile)
                             :context-version-major major
                             :context-version-minor minor)

         (glfw:get-current-context))
       ;; (create-context-by-version (version)
       ;;   (destructuring-bind (&optional major minor)
       ;;       (cepl.context:split-float-version version)
       ;;     (create-window major minor)))

       (search-for-context ()
         (let ((context nil))
           (loop :for (major minor) :in `((4 5) (4 4) (4 3)
                                          (4 2) (4 1) (4 0)
                                          (3 3))
              :until context
              :do (setf context (create-window major minor)))
           (assert context)
           context)))

    (search-for-context)))

(defun destroy-glfw-surface (surface)
  (glfw:destroy-window surface))

(defun glfw-surface-size (win-handle)
  (glfw:get-window-size win-handle))

(defun glfw-set-surface-size (win-handle width height)
  (glfw:set-window-size win-handle width height))

(defun glfw-surface-fullscreen-p (surface)
  (declare (ignore surface))
  nil)

(defun glfw-set-surface-fullscreen (surface state)
  (declare (ignore surface state))
  t)

(defun glfw-surface-title (surface)
  (declare (ignore surface))
  nil)

(defun glfw-set-surface-title (surface title)
  (glfw:set-window-title surface title))

;;----------------------------------------------------------------------

(defclass glfw-api (cepl.host:api-1)
  (;;
   (supports-multiple-contexts-p :initform nil)
   ;;
   (supports-multiple-surfaces-p :initform t)
   ;;
   (init-function :initform #'glfw-init)
   ;;
   (shutdown-function :initform #'glfw-shutdown)
   ;;
   (make-surface-function :initform #'make-glfw-surface)
   ;;
   (destroy-surface-function :initform #'destroy-glfw-surface)
   ;;
   (make-context-function :initform #'make-glfw-context)
   ;;
   (step-function :initform #'glfw-step-v1)
   ;;
   (register-event-callback-function :initform #'glfw-register-listener)
   ;;
   (swap-function :initform #'glfw-swap)
   ;;
   (surface-size-function :initform #'glfw-surface-size)
   ;;
   (make-context-current-function :initform #'glfw-make-current)
   ;;
   (set-surface-size-function :initform #'glfw-set-surface-size)
   ;;
   (surface-fullscreen-p-function :initform #'glfw-surface-fullscreen-p)
   ;;
   (set-surface-fullscreen-function :initform #'glfw-set-surface-fullscreen)
   ;;
   (surface-title-function :initform #'glfw-surface-title)
   ;;
   (set-surface-title-function :initform #'glfw-set-surface-title)))

(register-host 'glfw-api)

;;----------------------------------------------------------------------

(defun (setf vsync) (boolean)
  (warn "Sorry setting vsync is not supported")
  boolean)
