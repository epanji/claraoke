(cl:in-package #:claraoke-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color (ASS color)
;;;
(defun html-color-p (string)
  (and (stringp string)
       (< 1 (length string))
       (char-equal #\# (elt string 0))
       (case (length string)
         ;; #000 rgb
         (4 t)
         ;; #0000 rgba
         (5 t)
         ;; #000000 rrggbb
         (7 t)
         ;; #00000000 rrggbbaa
         (9 t)
         (otherwise nil))
       (every (lambda (c)
                (digit-char-p c 16))
              (subseq string 1))))

(defun ass-color-p (string)
  (and (stringp string)
       (< 2 (length string))
       (string-equal "&H" (subseq string 0 2))
       (case (length string)
         ;; &H000& or &H0000
         (6 (and (or (digit-char-p (elt string 5) 16)
                     (char-equal #\& (elt string 5)))
                 (every (lambda (c)
                          (digit-char-p c 16))
                        (subseq string 2 5))))
         ;; &H000000&
         (9 (and (char-equal #\& (elt string 8))
                 (every (lambda (c)
                          (digit-char-p c 16))
                        (subseq string 2 8))))
         ;; &H00000000
         (10 (and (char-not-equal #\& (elt string 9))
                  (every (lambda (c)
                           (digit-char-p c 16))
                         (subseq string 2))))
         (otherwise nil))))

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
             (otherwise (return-from html-color
                          (claraoke:rgb 255 255 255))))))
    (claraoke:rgb (dec-from-hexstring string 0 :digit c)
                  (dec-from-hexstring string 1 :digit c)
                  (dec-from-hexstring string 2 :digit c)
                  (when (or (= 5 (length string))
                            (= 9 (length string)))
                    (dec-from-hexstring string 3 :digit c)))))

(defun ass-color (string)
  (check-type string string)
  (let* ((ln (length string))
         (lc (elt string (1- ln)))
         (dg (round (- ln 2) 4))
         (decs (list (dec-from-hexstring string 0 :skip 2 :digit dg)
                     (dec-from-hexstring string 1 :skip 2 :digit dg)
                     (dec-from-hexstring string 2 :skip 2 :digit dg)
                     (when (and (or (= 6 ln) (= 10 ln))
                                (char-not-equal #\& lc))
                       (dec-from-hexstring string 3 :skip 2 :digit dg))))
         (rgba (reverse (remove nil decs))))
    (apply 'claraoke:rgb rgba)))

(defun integer-to-color (ub32)
  (check-type ub32 (unsigned-byte 32))
  (let ((red (ldb (byte 8 0) ub32))
        (green (ldb (byte 8 8) ub32))
        (blue (ldb (byte 8 16) ub32))
        (alpha (ldb (byte 8 24) ub32)))
    (claraoke:rgb red green blue (if (zerop alpha) nil alpha))))

(defun color-to-integer (color)
  (check-type color color)
  (let ((ub32 #x00000000))
    (setf (ldb (byte 8 0) ub32) (claraoke:red color))
    (setf (ldb (byte 8 8) ub32) (claraoke:green color))
    (setf (ldb (byte 8 16) ub32) (claraoke:blue color))
    (setf (ldb (byte 8 24) ub32) (or (claraoke:alpha color) 0))
    ub32))

(defvar *color-names* (make-hash-table :test 'equalp))

(defun normalize-color-name (string)
  (check-type string string)
  (let ((not-alphanumericp (complement (function alphanumericp))))
    (string-upcase (remove-if not-alphanumericp string))))

(defun keyword-from-name (string)
  (check-type string string)
  (intern (normalize-color-name string) :keyword))

(defun register-color-name (name color)
  (check-type name string)
  (check-type color string)
  (setf (gethash (keyword-from-name name) *color-names*)
        (color-to-integer (html-color color))))

(defun color-from-name (string)
  (check-type string string)
  (let ((value (gethash (keyword-from-name string) *color-names* nil)))
    (unless (null value)
      (integer-to-color value))))

(defmethod claraoke:color ((object string))
  (cond ((html-color-p object)
         (html-color object))
        ((ass-color-p object)
         (ass-color object))
        (t (or (color-from-name object)
               (claraoke:color 0)))))

(defmethod claraoke:color ((object integer))
  (let ((ub32 (mod (abs object) #xFFFFFFFF)))
    (integer-to-color ub32)))

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
  (claraoke:colorstring (claraoke:color color)))

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
;;; Color integer
;;;
(defmethod claraoke:colorinteger ((object color))
  (color-to-integer object))

(defmethod claraoke:colorinteger (object)
  (claraoke:colorinteger (claraoke:color object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color integer predicate
;;;
(defmethod claraoke:colorintegerp ((object integer))
  (typep object '(unsigned-byte 32)))

(defmethod claraoke:colorintegerp (object)
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
         (c (min 2 (- (length (string-right-trim "&" object)) s)))
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
  (typep object '(integer 0 255)))

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
       (<= 3 (length object) 5)
       (string-equal "&H" (subseq object 0 2))
       (case (length object)
         ;; &H0
         (3 (digit-char-p (elt object 2) 16))
         ;; &H00
         (4 (every (lambda (c)
                     (digit-char-p c 16))
                   (subseq object 2)))
         ;; &H00&
         (5 (and (char-equal #\& (elt object 4))
                 (every (lambda (c)
                          (digit-char-p c 16))
                        (subseq object 2 4))))
         (otherwise nil))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Synch color
;;;
(defmethod claraoke:synch-color ((color color) (source color))
  (setf (claraoke:red color) (claraoke:red source))
  (setf (claraoke:green color) (claraoke:green source))
  (setf (claraoke:blue color) (claraoke:blue source))
  (setf (claraoke:alpha color) (claraoke:alpha source))
  color)

(defmethod claraoke:synch-color ((color color) source)
  (claraoke:synch-color color (claraoke:color source)))

(defmethod claraoke:synch-color (color source)
  (error 'claraoke:object-must-be-color :object color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Increase color
;;;
(defmethod claraoke:increase-color ((object color) addition)
  (let ((source (claraoke:increase-color (claraoke:colorinteger object) addition)))
    (claraoke:synch-color object source)))

(defmethod claraoke:increase-color ((object integer) (addition integer))
  (claraoke:color (+ object addition)))

(defmethod claraoke:increase-color ((object integer) addition)
  (claraoke:increase-color object (claraoke:colorinteger addition)))

(defmethod claraoke:increase-color (object addition)
  (claraoke:increase-color (claraoke:colorinteger object) addition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrease color
;;;
(defmethod claraoke:decrease-color ((object color) subtraction)
  (let ((source (claraoke:decrease-color (claraoke:colorinteger object) subtraction)))
    (claraoke:synch-color object source)))

(defmethod claraoke:decrease-color ((object integer) (subtraction integer))
  (claraoke:color (- object subtraction)))

(defmethod claraoke:decrease-color ((object integer) subtraction)
  (claraoke:decrease-color object (claraoke:colorinteger subtraction)))

(defmethod claraoke:decrease-color (object subtraction)
  (claraoke:decrease-color (claraoke:colorinteger object) subtraction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color bitwise
;;; logand, logandc1, logandc2, logeqv, logior, lognand, lognor, logorc1, logorc2, logxor
;;;
(defmethod claraoke:bitwise-color ((bitwise function) (object1 integer) (object2 integer))
  (claraoke:color (funcall bitwise object1 object2)))

(defmethod claraoke:bitwise-color ((bitwise function) (object1 integer) object2)
  (claraoke:bitwise-color bitwise  object1 (claraoke:colorinteger object2)))

(defmethod claraoke:bitwise-color ((bitwise function) object1 object2)
  (claraoke:bitwise-color bitwise (claraoke:colorinteger object1) object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :and)) object1 object2)
  (claraoke:bitwise-color #'logand object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :andc1)) object1 object2)
  (claraoke:bitwise-color #'logandc1 object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :andc2)) object1 object2)
  (claraoke:bitwise-color #'logandc2 object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :eqv)) object1 object2)
  (claraoke:bitwise-color #'logeqv object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :ior)) object1 object2)
  (claraoke:bitwise-color #'logior object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :nand)) object1 object2)
  (claraoke:bitwise-color #'lognand object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :nor)) object1 object2)
  (claraoke:bitwise-color #'lognor object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :orc1)) object1 object2)
  (claraoke:bitwise-color #'logorc1 object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :orc2)) object1 object2)
  (claraoke:bitwise-color #'logorc2 object1 object2))

(defmethod claraoke:bitwise-color ((bitwise (eql :xor)) object1 object2)
  (claraoke:bitwise-color #'logxor object1 object2))

(defmethod claraoke:bitwise-color (bitwise object1 object2)
  (when (functionp bitwise)
    (claraoke:bitwise-color bitwise object1 object2)))

