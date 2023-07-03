(asdf:defsystem cl-ktx
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An implementation of the Khronos KTX image file format"
  :homepage "https://shinmera.github.io/cl-ktx/"
  :bug-tracker "https://github.com/shinmera/cl-ktx/issues"
  :source-control (:git "https://github.com/shinmera/cl-ktx.git")
  :serial T
  :components ((:file "package")
               (:file "ktx")
               (:file "documentation"))
  :depends-on (:binary-structures
               :cl-opengl
               :trivial-features
               :documentation-utils))
