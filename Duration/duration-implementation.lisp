(cl:in-package #:claraoke-duration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration
;;;
(defun extradigit (string)
  (check-type string (or null string))
  (when (null string)
    (return-from extradigit (list "0" "0")))
  (multiple-value-bind (c s e)
      (values-list (coerce string 'list))
    (let ((cs (with-output-to-string (stream)
                (unless (null c)
                  (princ c stream))
                (unless (null s)
                  (princ s stream))))
          (ed (if (null e) "0" (string e))))
      (list cs ed))))

(defun duration (string)
  (check-type string string)
  (let* ((sbc (claraoke-internal:split-by-char #\: string))
         (rsbc (reverse sbc))
         (s.cse (first rsbc))
         (m (claraoke-internal:integer-from-string (second rsbc)))
         (h (claraoke-internal:integer-from-string (third rsbc)))
         (sbd (claraoke-internal:split-by-char #\. s.cse))
         (s (claraoke-internal:integer-from-string (first sbd)))
         (cse (extradigit (second sbd)))
         (cs (claraoke-internal:integer-from-string (first cse)))
         (e (claraoke-internal:integer-from-string (second cse))))
    (check-type h integer)
    (check-type m (integer 0 59))
    (check-type s (integer 0 59))
    (check-type cs (integer 0 99))
    (check-type e (integer 0 9))
    (make-instance 'duration :h h :m m :s s :cs cs :ed e)))

(defmethod claraoke:duration ((object string))
  (if (claraoke:durationintegerp object)
      (claraoke:duration (claraoke-internal:integer-from-string object))
      (duration object)))

(defmethod claraoke:duration ((object integer))
  (let ((cssmh '())
        (hmscs (list (* 60 60 100) (* 60 100) 100 1))
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
  (claraoke:durationstring (claraoke:duration object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration string predicate
;;;
(defmethod claraoke:durationstringp ((object string))
  (let ((min (length object))
        (pos nil))
    (and (<= 10 min)
         (setf pos (position #\: object))
         (every 'digit-char-p (subseq object 0 pos))
         (digit-char-p (elt object (+ pos 1)))
         (digit-char-p (elt object (+ pos 2)))
         (char= #\: (elt object (+ pos 3)))
         (digit-char-p (elt object (+ pos 4)))
         (digit-char-p (elt object (+ pos 5)))
         (char= #\. (elt object (+ pos 6)))
         (digit-char-p (elt object (+ pos 7)))
         (digit-char-p (elt object (+ pos 8)))
         (or (= min (+ pos 9))
             (and (digit-char-p (elt object (+ pos 9)))
                  (= min (+ pos 10)))))))

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
;;; Synch duration
;;;
(defmethod claraoke:synch-duration ((object duration) (source duration))
  (setf (claraoke:hours object) (claraoke:hours source))
  (setf (claraoke:minutes object) (claraoke:minutes source))
  (setf (claraoke:seconds object) (claraoke:seconds source))
  (setf (claraoke:centiseconds object) (claraoke:centiseconds source))
  (setf (claraoke:extradigit object) (claraoke:extradigit source))
  object)

(defmethod claraoke:synch-duration ((object duration) source)
  (claraoke:synch-duration object (claraoke:duration source)))

(defmethod claraoke:synch-duration (duration source)
  (error 'claraoke:object-must-be-duration :object duration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Increase duration
;;;
(defmethod claraoke:increase-duration ((duration duration) (addition duration))
  (multiple-value-bind (quo rem)
      (floor (+ (claraoke:extradigit duration)
                (claraoke:extradigit addition))
             10)
    (let ((source (claraoke:duration
                   (+ (claraoke:durationinteger duration)
                      (claraoke:durationinteger addition)
                      quo))))
      (setf (claraoke:extradigit source) rem)
      (claraoke:synch-duration duration source))))

(defmethod claraoke:increase-duration ((duration duration) addition)
  (claraoke:increase-duration duration (claraoke:duration addition)))

(defmethod claraoke:increase-duration (duration addition)
  (error 'claraoke:object-must-be-duration :object duration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrease duration
;;;
(defmethod claraoke:decrease-duration ((duration duration) (subtraction duration))
  (multiple-value-bind (quo rem)
      (floor (- (claraoke:extradigit duration)
                (claraoke:extradigit subtraction))
             10)
    (let ((source (claraoke:duration
                   (- (claraoke:durationinteger duration)
                      (claraoke:durationinteger subtraction)
                      (abs quo)))))
      (setf (claraoke:extradigit source) rem)
      (claraoke:synch-duration duration source))))

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

