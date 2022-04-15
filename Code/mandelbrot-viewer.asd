(asdf:defsystem :mandelbrot-viewer
  :depends-on (:eazy-opencl :ieee-floats :mcclim)
  :serial t
  :components ((:file "package")
               (:file "gpu")
               (:file "clim")))
