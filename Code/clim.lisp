(in-package :mandelbrot-viewer)

(defun blit (buffer pane)
  (clim:draw-pattern* pane
                      (make-instance 'clim-extensions:image-pattern
                                     :array buffer)
                      0 0))

(defclass mandelbrot-pane (clim:application-pane)
  ((record :initform nil :accessor record)))

(clim:define-application-frame mandelbrot-app ()
  ((scale :initform 1.0 :accessor scale)
   (gpu :initarg :gpu :reader gpu)
   (width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (buffer :initarg :framebuffer :reader framebuffer))
  (:panes
   (output mandelbrot-pane
           :incremental-redisplay nil
           :display-function 'draw-mandelbrot))
  (:menu-bar nil)
  (:layouts
   (default output)))

(defun draw-mandelbrot (frame pane)
  (declare (ignore frame))
  (when (null (record pane))
    (setf (record pane)
          (clim:updating-output (pane)
            (let* ((scale (scale clim:*application-frame*))
                   (gpu (gpu clim:*application-frame*))
                   (*width* (width clim:*application-frame*))
                   (*height* (height clim:*application-frame*))
                   (aspect-ratio (float (/ *width* *height*)))
                   (x1 (- -1.250411215d0 (* aspect-ratio scale)))
                   (x2 (+ -1.250411215d0 (* aspect-ratio scale)))
                   (y1 (- -0.072562344d0 scale)) (y2 (+ -0.072562344d0 scale))
                   (buffer (time (run-gpu gpu (framebuffer clim:*application-frame*)
                                          x1 x2 y1 y2 1000))))
              (blit buffer pane)
              (setf (scale clim:*application-frame*) (* scale 0.95))))))
  (clim:redisplay (record pane) pane)
  (clim-internals::schedule-event pane
                                  (make-instance 'clim:timer-event
                                                 :sheet pane)
                                  0.01))

(defun animate (platform-n device-n &key (width 1920) (height 1080))
  (clim:run-frame-top-level
   (clim:make-application-frame 'mandelbrot-app
                                :width width
                                :height height
                                :framebuffer (make-array (list height width)
                                                         :element-type '(unsigned-byte 32))
                                :gpu (choose-device platform-n device-n))))

(defmethod clim:handle-event ((pane mandelbrot-pane) (event clim:timer-event))
  (draw-mandelbrot nil pane))
