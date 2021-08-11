(cl:in-package #:cl-user)

(defpackage #:claraoke-user
  (:use #:common-lisp #:claraoke)
  (:export
   #:claraoke-version
   #:claraoke-subtitle-to-file
   #| ... |#))

(cl:in-package #:claraoke-user)

(defun claraoke-version ()
  (claraoke-internal:version))

(defun claraoke-subtitle-to-file (object filespec &key (if-exists :rename) (if-does-not-exist :create))
  (check-type filespec pathname)
  (with-open-file (stream filespec :direction :output
                                   :if-exists if-exists
                                   :if-does-not-exist if-does-not-exist)
    (print-script object stream)))

