;;;; cepl.glfw.asd

(asdf:defsystem #:cepl.glfw
  :description "GLFW3 host for cepl"
  :author "Daniel Parnell <me@danielparnell.com>"
  :license "BSD 2 Clause"
  :depends-on (#:cffi
               #:cl-opengl
               #:cl-glfw3
               #:cepl)
  :serial t
  :components ((:file "package")
               (:file "cepl.glfw")))
