#|
 This file is a part of cl-ktx
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.ktx)

(bs:define-io-structure kv-entry
  (size uint32)
  (kv-string (string (bs:slot size)))
  (NIL (vector uint8 (- 3 (mod (+ 3 (bs:slot size)) 4)))))

(defmethod print-object ((entry kv-entry) stream)
  (print-unreadable-object (entry stream :type T)
    (let* ((string (kv-entry-kv-string entry))
           (sep (position #\Nul string))
           (key (subseq string 0 sep))
           (val (subseq string (1+ sep))))
      (format stream "~a: ~a" key val))))

(bs:define-io-structure mipmap-level
  (size uint32 :align 4)
  ;; FIXME: this is not right because for cube maps the spec is really dumb.
  (data (vector uint8 (bs:slot size))))

(defmethod print-object ((level mipmap-level) stream)
  (print-unreadable-object (level stream :type T :identity T)
    (format stream "~d bytes" (size level))))

(bs:define-io-structure file
  #(#xAB #x4B #x54 #x58 #x20 #x31 #x31 #xBB #x0D #x0A #x1A #x0A)
  (endianness (case uint32
                (#x04030201 #+little-endian :little-endian #+big-endian :big-endian)
                (#x01020304 #+little-endian :big-endian #+big-endian :little-endian)))
  (gl-type uint32)
  (gl-type-size uint32)
  (gl-format uint32)
  (gl-internal-format uint32)
  (gl-base-internal-format uint32)
  (width uint32)
  (height uint32)
  (depth uint32)
  (array-element-count uint32)
  (face-count uint32)
  (mip-count uint32)
  (kv-store-size uint32)
  (kv-store (vector uint8 (bs:slot kv-store-size)))
  (mipmaps (vector mipmap-level (max 1 (bs:slot mip-count)))))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~ax~ax~a ~a ~a"
            (width file) (max 1 (height file)) (max 1 (depth file))
            (cffi:foreign-enum-keyword '%gl::enum (gl-type file))
            (cffi:foreign-enum-keyword '%gl::enum (gl-format file)))))

(defmacro define-accessors (type &rest names)
  `(progn ,@(loop for name in names
                  collect `(defmethod ,name ((entry ,type))
                             (,(intern (format NIL "~a-~a" type name)) entry))
                  collect `(defmethod (setf ,name) (value (entry ,type))
                             (setf (,(intern (format NIL "~a-~a" type name)) entry) value)))))

(define-accessors file endianness gl-type gl-type-size gl-format gl-internal-format gl-base-internal-format width height depth array-element-count face-count mip-count mipmaps)
(define-accessors mipmap-level size data)

(defmacro define-gl-accessors (type &rest names)
  `(progn ,@(loop for name in names
                  collect `(defmethod (setf ,name) (value (entry ,type))
                             (setf (,(intern (format NIL "~a-~a" type name)) entry)
                                   (cffi:foreign-enum-value '%gl::enum value))))))

(define-gl-accessors file gl-type gl-format gl-internal-format gl-base-internal-format)

(defmethod kv-store ((file file))
  (let ((table (make-hash-table :test 'equal))
        (vector (file-kv-store file))
        (start 0)
        (end (file-kv-store-size file)))
    (loop while (< start end)
          do (multiple-value-bind (entry next) (read-io-octet-vector-kv-entry vector start end)
               (setf start next)
               (let* ((string (kv-entry-kv-string entry))
                      (sep (position #\Nul string))
                      (key (subseq string 0 sep))
                      (val (subseq string (1+ sep))))
                 (setf (gethash key table) val))))
    table))

(defmethod (setf kv-store) ((table hash-table) (file file))
  (setf (kv-store file) (alexandria:hash-table-alist table))
  table)

(defmethod (setf kv-store) ((table cons) (file file))
  (let ((entries ())
        (size 0))
    (loop for (key . val) in table
          for koctets = (babel:string-size-in-octets key :encoding :utf-8)
          for voctets = (babel:string-size-in-octets val :encoding :utf-8)
          for strlen = (+ koctets 1 voctets)
          for padding = (- 3 (mod (+ 3 strlen) 4))
          do (push (make-kv-entry :size strlen :kv-string (concatenate 'string key '(#\Nul) val)) entries)
             (incf size (+ 4 strlen padding)))
    (let ((vector (make-array size :element-type '(unsigned-byte 8))))
      (loop for start = 0 then next
            for entry in (nreverse entries)
            for next = (write-io-octet-vector-kv-entry entry vector start size))
      (setf (file-kv-store file) vector)
      (setf (file-kv-store-size file) size))
    table))
