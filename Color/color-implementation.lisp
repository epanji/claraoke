(cl:in-package #:claraoke-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color (ASS color)
;;;
(defun html-color-p (string)
  (and (stringp string)
       (or (= 4 (length string))
           (= 5 (length string))
           (= 7 (length string))
           (= 9 (length string)))
       (char-equal #\# (elt string 0))))

(defun ass-color-p (string)
  (and (stringp string)
       (if (= 9 (length string))
           (char-equal #\& (elt string 8))
           (= 10 (length string)))
       (string-equal "&H" (subseq string 0 2))))

(defun dec-from-hexstring (string index &key (digit 2) (skip 1))
  (check-type string string)
  (check-type index (integer 0 3))
  (check-type digit (integer 1 2))
  (check-type skip (integer 0 9))
  (let* ((multiplier (if (= 1 digit) 17 1))
         (target (* index digit))
         (start (+ skip target))
         (end (+ skip target digit)))
    (* multiplier (parse-integer string :start start :end end :radix 16))))

(defun html-color (string)
  (check-type string string)
  (let ((c (case (length string)
             ((4 5) 1)
             ((7 9) 2)
             (t (return-from html-color (claraoke:rgb 255 255 255))))))
    (claraoke:rgb (dec-from-hexstring string 0 :digit c)
                  (dec-from-hexstring string 1 :digit c)
                  (dec-from-hexstring string 2 :digit c)
                  (when (or (= 5 (length string))
                            (= 9 (length string)))
                    (dec-from-hexstring string 3 :digit c)))))

(defun ass-color (string)
  (check-type string string)
  (let* ((rgba (list (dec-from-hexstring string 0 :skip 2)
                     (dec-from-hexstring string 1 :skip 2)
                     (dec-from-hexstring string 2 :skip 2)
                     (when (= 10 (length string))
                       (dec-from-hexstring string 3 :skip 2))))
         (agbr (reverse (remove nil rgba))))
    (apply 'claraoke:rgb agbr)))

(defvar *color-names* (make-hash-table :test 'equalp))

(defun normalize-color-name (string)
  (check-type string string)
  (let ((not-alpha-char-p (complement (function alpha-char-p))))
    (string-upcase (remove-if not-alpha-char-p string))))

(defun keyword-from-name (string)
  (check-type string string)
  (intern (normalize-color-name string) :keyword))

(defun register-color-name (name color)
  (check-type name string)
  (check-type color string)
  (setf (gethash (keyword-from-name name) *color-names*)
        (html-color color)))

(defun color-from-name (string)
  (check-type string string)
  (identity (gethash (keyword-from-name string) *color-names* nil)))

(defmethod claraoke:color ((object string))
  (cond ((html-color-p object)
         (html-color object))
        ((ass-color-p object)
         (ass-color object))
        (t (or (color-from-name object)
               (claraoke:color 0)))))

(defmethod claraoke:color ((object (eql 0)))
  (claraoke:rgb 0 0 0 nil))

(defmethod claraoke:color ((object color))
  object)

(defmethod claraoke:color (color)
  (error 'claraoke:failed-to-create-color :object color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color predicate
;;;
(defmethod claraoke:colorp ((object color))
  t)

(defmethod claraoke:colorp (object)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color string
;;;
(defmethod claraoke:colorstring ((object color))
  (let ((red (claraoke:red object))
        (green (claraoke:green object))
        (blue (claraoke:blue object))
        (alpha (claraoke:alpha object)))
    (if (null alpha)
        (format nil "&H~2,'0X~2,'0X~2,'0X&" blue green red)
        (format nil "&H~2,'0X~2,'0X~2,'0X~2,'0X" alpha blue green red))))

(defmethod claraoke:colorstring (color)
  (error 'claraoke:object-must-be-color :object color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color string predicate
;;;
(defmethod claraoke:colorstringp ((object string))
  (ass-color-p object))

(defmethod claraoke:colorstringp (object)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alpha
;;;
(defmethod claraoke:alpha ((object integer))
  (check-type object (integer 0 255))
  object)

(defmethod claraoke:alpha ((object string))
  (let* ((s (case (elt object 0)
              (#\# 1)
              (#\& (if (char-equal #\h (elt object 1)) 2 1))
              (otherwise 0)))
         (c (min 2 (- (length object) s)))
         (i (dec-from-hexstring object 0 :digit c :skip s)))
    (claraoke:alpha i)))

(defmethod claraoke:alpha ((object ratio))
  (let ((i (ceiling (* 255 object))))
    (claraoke:alpha i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alpha predicate
;;;
(defmethod claraoke:alphap ((object integer))
  (the (integer 0 255) object))

(defmethod claraoke:alphap (object)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alpha string
;;;
(defmethod claraoke:alphastring (object)
  (let ((i (claraoke:alpha object)))
    (values (format nil "&H~2,'0X&" i) i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Alpha string predicate
;;;
(defmethod claraoke:alphastringp ((object string))
  (and (stringp object)
       (<= 4 (length object))
       (string-equal "&H" (subseq object 0 2))
       (parse-integer (subseq object 2 4) :radix 16)))

(defmethod claraoke:alphastringp (object)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RGB (Red Green Blue)
;;;
(defmethod claraoke:rgb ((red integer) (green integer) (blue integer) &optional alpha)
  (check-type red (integer 0 255))
  (check-type green (integer 0 255))
  (check-type blue (integer 0 255))
  (check-type alpha (or null (integer 0 255)))
  (make-instance 'color :red red :green green :blue blue :alpha alpha))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Random color
;;;
(defmethod claraoke:random-color (&optional alpha)
  (check-type alpha (or null string ratio (integer 0 255)))
  (claraoke:rgb (random 255) (random 255) (random 255)
                (typecase alpha
                  ((or null integer) alpha)
                  (otherwise (claraoke:alpha alpha)))))

