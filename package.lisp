#|
 This file is a part of cl-ktx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.ktx
  (:use #:cl)
  (:local-nicknames
   (#:bs #:org.shirakumo.binary-structures))
  (:shadow #:type)
  (:use #:cl #:org.shirakumo.binary-structures.types)
  (:export
   #:mipmap-level
   #:make-mipmap-level
   #:file
   #:create-file
   #:read-file
   #:write-file
   #:endianness
   #:gl-type
   #:gl-type-size
   #:gl-format
   #:gl-internal-format
   #:gl-base-internal-format
   #:width
   #:height
   #:depth
   #:array-element-count
   #:face-count
   #:mip-count
   #:mipmaps
   #:size
   #:data
   #:kv-store))
