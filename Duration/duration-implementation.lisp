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
         (s.ms (first rssc))
         (m (integer-from-string (second rssc)))
         (h (integer-from-string (third rssc)))
         (ssd (split-sequence:split-sequence #\. s.ms :remove-empty-subseqs t))
         (s (integer-from-string (first ssd)))
         (ms (integer-from-string (second ssd))))
    (check-type h integer)
    (check-type m (integer 0 59))
    (check-type s (integer 0 59))
    (check-type ms (integer 0 999))
    (make-instance 'duration :h h :m m :s s :ms ms)))

(defmethod claraoke:duration ((duration string))
  (if (claraoke:durationintegerp duration)
      (claraoke:duration (integer-from-string duration))
      (duration duration)))

(defmethod claraoke:duration ((duration integer))
  (let ((mssmh '())
        (hmsms '(#.(* 60 60 1000) #.(* 60 1000) #.(* 1000) 1))
        (initial-value (max 0 duration)))
    (reduce (lambda (last-value divisor)
              (multiple-value-bind (quotient remainder)
                  (floor last-value divisor)
                (push quotient mssmh) remainder))
            hmsms
            :initial-value initial-value)
    (destructuring-bind (ms s m h) mssmh
      (make-instance 'duration :h h :m m :s s :ms ms))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration string
;;;
(defmethod claraoke:durationstring ((duration duration))
  (format nil "~2,'0D:~2,'0D:~2,'0D.~3,'0D"
          (claraoke:hours duration)
          (claraoke:minutes duration)
          (claraoke:seconds duration)
          (claraoke:miliseconds duration)))

(defmethod print-object ((duration duration) stream)
  (prin1 (claraoke:durationstring duration) stream))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration integer (miliseconds)
;;;
(defmethod claraoke:durationinteger ((duration duration))
  (+ (* 1 (claraoke:miliseconds duration))
     (* 1000 (claraoke:seconds duration))
     (* #.(* 60 1000) (claraoke:minutes duration))
     (* #.(* 60 60 1000) (claraoke:hours duration))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration integer predicate
;;;
(defmethod claraoke:durationintegerp ((duration integer))
  t)

(defmethod claraoke:durationintegerp ((duration string))
  (every 'digit-char-p duration))

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

