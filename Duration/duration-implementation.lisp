(cl:in-package #:claraoke-duration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration
;;;
(defun duration (string)
  (check-type string string)
  (let* ((ssc (split-sequence:split-sequence #\: string :remove-empty-subseqs t))
         (rssc (reverse ssc))
         (s.cs (first rssc))
         (m (claraoke-internal:integer-from-string (second rssc)))
         (h (claraoke-internal:integer-from-string (third rssc)))
         (ssd (split-sequence:split-sequence #\. s.cs :remove-empty-subseqs t))
         (s (claraoke-internal:integer-from-string (first ssd)))
         (cs (claraoke-internal:integer-from-string (second ssd))))
    (check-type h integer)
    (check-type m (integer 0 59))
    (check-type s (integer 0 59))
    (check-type cs (integer 0 99))
    (make-instance 'duration :h h :m m :s s :cs cs)))

(defmethod claraoke:duration ((object string))
  (if (claraoke:durationintegerp object)
      (claraoke:duration (claraoke-internal:integer-from-string object))
      (duration object)))

(defmethod claraoke:duration ((object integer))
  (let ((cssmh '())
        (hmscs `(,(* 60 60 100) ,(* 60 100) ,(* 100) 1))
        (initial-value (max 0 object)))
    (reduce (lambda (last-value divisor)
              (multiple-value-bind (quotient remainder)
                  (floor last-value divisor)
                (push quotient cssmh) remainder))
            hmscs
            :initial-value initial-value)
    (destructuring-bind (cs s m h) cssmh
      (make-instance 'duration :h h :m m :s s :cs cs))))

(defmethod claraoke:duration ((object duration))
  object)

(defmethod claraoke:duration (duration)
  (error 'claraoke:failed-to-create-duration :object duration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration predicate
;;;
(defmethod claraoke:durationp ((object duration))
  t)

(defmethod claraoke:durationp (object)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration string
;;;
(defmethod claraoke:durationstring ((object duration))
  (format nil "~D:~2,'0D:~2,'0D.~2,'0D"
          (claraoke:hours object)
          (claraoke:minutes object)
          (claraoke:seconds object)
          (claraoke:centiseconds object)))

(defmethod claraoke:durationstring (object)
  (error 'claraoke:object-must-be-duration :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration string predicate
;;;
(defvar +duration-characters+ "0123456789.:")

(defun duration-character-p (char)
  (when (find char +duration-characters+) t))

(defmethod claraoke:durationstringp ((object string))
  (and (every 'duration-character-p (string-trim '(#\Space #\Tab) object))
       (when (ignore-errors (claraoke:duration object))
         t)))

(defmethod claraoke:durationstringp (object)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration integer (Centiseconds)
;;;
(defmethod claraoke:durationinteger ((object duration))
  (+ (* 1 (claraoke:centiseconds object))
     (* 100 (claraoke:seconds object))
     (* (* 60 100) (claraoke:minutes object))
     (* (* 60 60 100) (claraoke:hours object))))

(defmethod claraoke:durationinteger ((object string))
  (claraoke:durationinteger (claraoke:duration object)))

(defmethod claraoke:durationinteger ((object integer))
  object)

(defmethod claraoke:durationinteger (object)
  (error 'claraoke:failed-to-create-integer :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration integer predicate
;;;
(defmethod claraoke:durationintegerp ((object integer))
  t)

(defmethod claraoke:durationintegerp ((object string))
  (every 'digit-char-p object))

(defmethod claraoke:durationintegerp (object)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sync duration
;;;
(defmethod claraoke:sync-duration ((object duration) (source duration))
  (setf (claraoke:hours object) (claraoke:hours source))
  (setf (claraoke:minutes object) (claraoke:minutes source))
  (setf (claraoke:seconds object) (claraoke:seconds source))
  (setf (claraoke:centiseconds object) (claraoke:centiseconds source))
  object)

(defmethod claraoke:sync-duration ((object duration) source)
  (claraoke:sync-duration object (claraoke:duration source)))

(defmethod claraoke:sync-duration (duration source)
  (error 'claraoke:object-must-be-duration :object duration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Increase duration
;;;
(defmethod claraoke:increase-duration ((duration duration) (addition duration))
  (claraoke:sync-duration duration (+ (claraoke:durationinteger duration)
                                      (claraoke:durationinteger addition))))

(defmethod claraoke:increase-duration ((duration duration) addition)
  (claraoke:increase-duration duration (claraoke:duration addition)))

(defmethod claraoke:increase-duration (duration addition)
  (error 'claraoke:object-must-be-duration :object duration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrease duration
;;;
(defmethod claraoke:decrease-duration ((duration duration) (subtraction duration))
  (claraoke:sync-duration duration (- (claraoke:durationinteger duration)
                                      (claraoke:durationinteger subtraction))))

(defmethod claraoke:decrease-duration ((duration duration) subtraction)
  (claraoke:decrease-duration duration (claraoke:duration subtraction)))

(defmethod claraoke:decrease-duration (duration subtraction)
  (error 'claraoke:object-must-be-duration :object duration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration lessp
;;;
(defmethod claraoke:duration-lessp ((duration1 integer) (duration2 integer))
  (< duration1 duration2))

(defmethod claraoke:duration-lessp ((duration1 integer) duration2)
  (claraoke:duration-lessp duration1 (claraoke:durationinteger duration2)))

(defmethod claraoke:duration-lessp (duration1 duration2)
  (claraoke:duration-lessp (claraoke:durationinteger duration1) duration2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration greaterp
;;;
(defmethod claraoke:duration-greaterp ((duration1 integer) (duration2 integer))
  (> duration1 duration2))

(defmethod claraoke:duration-greaterp ((duration1 integer) duration2)
  (claraoke:duration-greaterp duration1 (claraoke:durationinteger duration2)))

(defmethod claraoke:duration-greaterp (duration1 duration2)
  (claraoke:duration-greaterp (claraoke:durationinteger duration1) duration2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration difference (Centiseconds)
;;;
(defmethod claraoke:duration-difference ((duration1 integer) (duration2 integer))
  (abs (- duration1 duration2)))

(defmethod claraoke:duration-difference ((duration1 integer) duration2)
  (claraoke:duration-difference duration1 (claraoke:durationinteger duration2)))

(defmethod claraoke:duration-difference (duration1 duration2)
  (claraoke:duration-difference (claraoke:durationinteger duration1) duration2))

