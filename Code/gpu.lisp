(in-package :mandelbrot-viewer)

(defvar *width* 1920)
(defvar *height* 1080)
(define-symbol-macro *array-size* (* *width* *height*))
(defvar *segments* 256)

(defun list-devices (platform n)
  (loop for device in (eazy-opencl.host:get-device-ids platform :device-type-default)
        for dn from 0
        do (format t "Device ~2d,~2d: ~a, ~4d MHz, ~4d MiB memory~%"
		   n dn
		   (eazy-opencl.host:get-device-info device :device-name)
		   (eazy-opencl.host:get-device-info device :device-max-clock-frequency)
		   (round (eazy-opencl.host:get-device-info device :device-global-mem-size)
			  #.(expt 2 20))))
  (values))

(defun list-platforms (&optional (*standard-output* *standard-output*))
  (loop for platform in (eazy-opencl.host:get-platform-ids)
        for n from 0
        do (format t "Platform ~1d: ~a from ~a, ~a~%"
		   n
		   (eazy-opencl.host:get-platform-info platform :platform-name)
		   (eazy-opencl.host:get-platform-info platform :platform-vendor)
		   (eazy-opencl.host:get-platform-info platform :platform-version))
           (list-devices platform n))
  (values))

(defstruct gpu
  platform
  device
  context
  result-buffer
  kernel
  queue)

(defun load-program (context device)
  (let ((program
          (eazy-opencl.host:create-program-with-source
           context
           (alexandria:read-file-into-string
            (asdf:system-relative-pathname :mandelbrot-viewer "mandelbrot.cl")))))
    (handler-case
        (eazy-opencl.host:build-program program :devices (list device))
      (eazy-opencl.bindings:opencl-error ()
        (cffi:with-foreign-array (length-box '%ocl:size-t (list 0))
          (%ocl:get-program-build-info
           program device
           :program-build-log
           0 (cffi:null-pointer) length-box)
          (let* ((length (cffi:mem-ref length-box :unsigned-long 0))
                 (buffer (make-array length :element-type '(unsigned-byte 8))))
            (cffi:with-pointer-to-vector-data (buffer-ptr buffer)
              (%ocl:get-program-build-info
             program device
               :program-build-log
               length buffer-ptr (cffi:null-pointer))
              (error "Compilation failed:~%~a"
                     (map 'string #'code-char buffer)))))))
    program))

(defun choose-device (platform-n &optional (device-n 0))
  (let* ((platform (elt (eazy-opencl.host:get-platform-ids) platform-n))
         (devices  (eazy-opencl.host:get-device-ids platform :device-type-default))
         (device  (elt devices device-n))
         (context  (eazy-opencl.host:create-context devices :context-platform platform)))
    (make-gpu
     :platform platform
     :device device
     :context context
     :result-buffer (eazy-opencl.host:create-buffer context :mem-write-only (* 4 *array-size*))
     :kernel (eazy-opencl.host:create-kernel (load-program context device) "mandel")
     :queue (eazy-opencl.host:create-command-queue-with-properties context device))))

(defun run-gpu (gpu framebuffer x1 x2 y1 y2 maximum-iterations)
  (let ((kernel (gpu-kernel gpu))
        (queue (gpu-queue gpu))
        (buffer (gpu-result-buffer gpu)))
    (macrolet ((args (&rest rest)
                 `(progn
                    ,@(loop for (type value) in rest
                            for n from 0
                            collect `(eazy-opencl.host:set-kernel-arg kernel ,n ,value ',type)))))
      (flet ((fl (n)
               ;; Apparently cl_double gets grovelled as long,
               ;; i.e. (signed-byte 64). >:(
               (let ((x (ieee-floats:encode-float64 (float n 1.0d0))))
                 (if (logtest x (ash 1 63)) ; MSB set?
                     (- x (ash 1 64))
                     x))))
        (cffi:with-pointer-to-vector-data (pointer (sb-ext:array-storage-vector framebuffer))
          (cffi:with-foreign-array (work-size '%ocl:size-t (list (* *segments* *height*)))
            (args (%ocl:uint *width*)
                  (%ocl:uint *height*)
                  (%ocl:uint maximum-iterations)
                  (%ocl:double (fl x1))
                  (%ocl:double (fl x2))
                  (%ocl:double (fl y1))
                  (%ocl:double (fl y2))
                  (%ocl:mem buffer))
            (%ocl:enqueue-nd-range-kernel queue kernel
                                          1 (cffi:null-pointer) work-size
                                          (cffi:null-pointer)
                                          0 (cffi:null-pointer) (cffi:null-pointer))
            (%ocl:enqueue-read-buffer queue buffer %ocl:true
                                      0 (* 4 *array-size*) pointer
                                      0 (cffi:null-pointer) (cffi:null-pointer))))))
    framebuffer))
