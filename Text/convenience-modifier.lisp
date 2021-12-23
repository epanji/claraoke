(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; After initialization
;;;
(defmethod initialize-instance :after
    ((instance modifier) &key arg1 arg2 arg3 arg4 arg5 arg6 arg7)
  (flet ((normalize (arg)
           (cond ((typep arg '(or cons modifier))
                  (with-output-to-string (str) (print-override arg str)))
                 ((claraoke:colorp arg)
                  (claraoke:colorstring arg))
                 (t arg))))
    (unless (null arg1)
      (setf (claraoke:arg1 instance) (normalize arg1)))
    (unless (null arg2)
      (setf (claraoke:arg2 instance) (normalize arg2)))
    (unless (null arg3)
      (setf (claraoke:arg3 instance) (normalize arg3)))
    (unless (null arg4)
      (setf (claraoke:arg4 instance) (normalize arg4)))
    (unless (null arg5)
      (setf (claraoke:arg5 instance) (normalize arg5)))
    (unless (null arg6)
      (setf (claraoke:arg6 instance) (normalize arg6)))
    (unless (null arg7)
      (setf (claraoke:arg7 instance) (normalize arg7)))))

