(cl:in-package #:claraoke-duration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration
;;;
(defun integer-from-string (string &optional (default 0))
  (or (parse-integer (string string) :junk-allowed t) default))

(defun duration (string)
  (check-type string string)
  (let* ((ssc (split-sequence:split-sequence #\: string :remove-empty-subseqs t))
         (rssc (reverse ssc))
         (s.cs (first rssc))
         (m (integer-from-string (second rssc)))
         (h (integer-from-string (third rssc)))
         (ssd (split-sequence:split-sequence #\. s.cs :remove-empty-subseqs t))
         (s (integer-from-string (first ssd)))
         (cs (integer-from-string (second ssd))))
    (check-type h integer)
    (check-type m (integer 0 59))
    (check-type s (integer 0 59))
    (check-type cs (integer 0 99))
    (make-instance 'duration :h h :m m :s s :cs cs)))

(defmethod claraoke:duration ((duration string))
  (if (claraoke:durationintegerp duration)
      (claraoke:duration (integer-from-string duration))
      (duration duration)))

(defmethod claraoke:duration ((duration integer))
  (let ((cssmh '())
        (hmscs '(#.(* 60 60 100) #.(* 60 100) #.(* 100) 1))
        (initial-value (max 0 duration)))
    (reduce (lambda (last-value divisor)
              (multiple-value-bind (quotient remainder)
                  (floor last-value divisor)
                (push quotient cssmh) remainder))
            hmscs
            :initial-value initial-value)
    (destructuring-bind (cs s m h) cssmh
      (make-instance 'duration :h h :m m :s s :cs cs))))

(defmethod claraoke:duration ((duration duration))
  duration)

(defmethod claraoke:duration ((duration null))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration predicate
;;;
(defmethod claraoke:durationp ((duration duration))
  t)

(defmethod claraoke:durationp (duration)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration string
;;;
(defmethod claraoke:durationstring ((duration duration))
  (format nil "~2,'0D:~2,'0D:~2,'0D.~2,'0D"
          (claraoke:hours duration)
          (claraoke:minutes duration)
          (claraoke:seconds duration)
          (claraoke:centiseconds duration)))

(defmethod print-object ((duration duration) stream)
  (princ (claraoke:durationstring duration) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration string predicate
;;;
(defvar +duration-characters+ "0123456789.:")

(defun duration-character-p (char)
  (when (find char +duration-characters+) t))

(defmethod claraoke:durationstringp ((string string))
  (and (every 'duration-character-p (string-trim '(#\space #\tab) string))
       (when (ignore-errors (claraoke:duration string))
         t)))

(defmethod claraoke:durationstringp (string)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration integer (centiseconds)
;;;
(defmethod claraoke:durationinteger ((duration duration))
  (+ (* 1 (claraoke:centiseconds duration))
     (* 100 (claraoke:seconds duration))
     (* #.(* 60 100) (claraoke:minutes duration))
     (* #.(* 60 60 100) (claraoke:hours duration))))

(defmethod claraoke:durationinteger ((duration string))
  (claraoke:durationinteger (claraoke:duration duration)))

(defmethod claraoke:durationinteger ((duration integer))
  duration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration integer predicate
;;;
(defmethod claraoke:durationintegerp ((duration integer))
  t)

(defmethod claraoke:durationintegerp ((duration string))
  (every 'digit-char-p duration))

(defmethod claraoke:durationintegerp (duration)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Increase duration
;;;
(defmethod claraoke:increase-duration ((duration duration) (addition duration))
  (claraoke:duration (+ (claraoke:durationinteger duration)
                        (claraoke:durationinteger addition))))

(defmethod claraoke:increase-duration ((duration string) addition)
  (claraoke:increase-duration (claraoke:duration duration) addition))

(defmethod claraoke:increase-duration (duration (addition string))
  (claraoke:increase-duration duration (claraoke:duration addition)))

(defmethod claraoke:increase-duration ((duration integer) addition)
  (claraoke:increase-duration (claraoke:duration duration) addition))

(defmethod claraoke:increase-duration (duration (addition integer))
  (claraoke:increase-duration duration (claraoke:duration addition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrease duration
;;;
(defmethod claraoke:decrease-duration ((duration duration) (subtraction duration))
  (claraoke:duration (- (claraoke:durationinteger duration)
                        (claraoke:durationinteger subtraction))))

(defmethod claraoke:decrease-duration ((duration string) subtraction)
  (claraoke:decrease-duration (claraoke:duration duration) subtraction))

(defmethod claraoke:decrease-duration (duration (subtraction string))
  (claraoke:decrease-duration duration (claraoke:duration subtraction)))

(defmethod claraoke:decrease-duration ((duration integer) subtraction)
  (claraoke:decrease-duration (claraoke:duration duration) subtraction))

(defmethod claraoke:decrease-duration (duration (subtraction integer))
  (claraoke:decrease-duration duration (claraoke:duration subtraction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration lessp
;;;
(defmethod claraoke:duration-lessp ((duration1 integer) (duration2 integer))
  (< duration1 duration2))

(defmethod claraoke:duration-lessp ((duration1 duration) duration2)
  (claraoke:duration-lessp (claraoke:durationinteger duration1) duration2))

(defmethod claraoke:duration-lessp (duration1 (duration2 duration))
  (claraoke:duration-lessp duration1 (claraoke:durationinteger duration2)))

(defmethod claraoke:duration-lessp ((duration1 string) duration2)
  (claraoke:duration-lessp (claraoke:duration duration1) duration2))

(defmethod claraoke:duration-lessp (duration1 (duration2 string))
  (claraoke:duration-lessp duration1 (claraoke:duration duration2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration greaterp
;;;
(defmethod claraoke:duration-greaterp ((duration1 integer) (duration2 integer))
  (> duration1 duration2))

(defmethod claraoke:duration-greaterp ((duration1 duration) duration2)
  (claraoke:duration-greaterp (claraoke:durationinteger duration1) duration2))

(defmethod claraoke:duration-greaterp (duration1 (duration2 duration))
  (claraoke:duration-greaterp duration1 (claraoke:durationinteger duration2)))

(defmethod claraoke:duration-greaterp ((duration1 string) duration2)
  (claraoke:duration-greaterp (claraoke:duration duration1) duration2))

(defmethod claraoke:duration-greaterp (duration1 (duration2 string))
  (claraoke:duration-greaterp duration1 (claraoke:duration duration2)))

