(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version
;;;
(setf (fdefinition 'claraoke-internal:version)
      (lambda ()
        "Return the CLARAOKE version."
        #.(let ((file (merge-pathnames "version.lisp-expr" (or *compile-file-pathname* *load-truename*))))
            (format nil "CLARAOKE v~A" (with-open-file (stream file) (read stream))))))

