(cl:in-package #:claraoke-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color (ASS color)
;;;
(defmethod claraoke:color ((color color))
  color)

(defmethod claraoke:color ((color null))
  nil)

(defmethod claraoke:color ((color (eql 0)))
  (claraoke:rgb 0 0 0 nil))

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

(defun dec-from-hexstring (string index &key (char 2) (skip 1))
  (check-type string string)
  (check-type index (integer 0 3))
  (check-type char (integer 1 2))
  (check-type skip (integer 0 9))
  (let ((multiplier (if (= 1 char) 17 1)))
    (* multiplier (parse-integer string
                                 :start (+ skip (* index char))
                                 :end   (+ skip (* (1+ index) char))
                                 :radix 16))))

(defun html-color (string)
  (check-type string string)
  (let ((c (case (length string)
             ((or 4 5) 1)
             ((or 7 9) 2)
             (t (return-from html-color (claraoke:rgb 255 255 255))))))
    (claraoke:rgb (dec-from-hexstring string 0 :char c)
                  (dec-from-hexstring string 1 :char c)
                  (dec-from-hexstring string 2 :char c)
                  (when (or (= 5 (length string))
                            (= 9 (length string)))
                    (dec-from-hexstring string 3 :char c)))))

(defun ass-color (string)
  (check-type string string)
  (let* ((rgba (list (dec-from-hexstring string 0 :skip 2)
                     (dec-from-hexstring string 1 :skip 2)
                     (dec-from-hexstring string 2 :skip 2)
                     (when (= 10 (length string))
                       (dec-from-hexstring string 3 :skip 2))))
         (agbr (reverse (remove nil rgba))))
    (apply 'claraoke:rgb agbr)))

(defmethod claraoke:color ((color string))
  (cond ((html-color-p color)
         (html-color color))
        ((ass-color-p color)
         (ass-color color))
        (t (claraoke:color 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color string
;;;
(defmethod claraoke:colorstring ((color color))
  (let ((red (claraoke:red color))
        (green (claraoke:green color))
        (blue (claraoke:blue color))
        (alpha (claraoke:alpha color)))
    (if (null alpha)
        (format nil "&H~2,'0X~2,'0X~2,'0X&" blue green red)
        (format nil "&H~2,'0X~2,'0X~2,'0X~2,'0X" alpha blue green red))))

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
(defmethod claraoke:random-color (&optional (alpha nil))
  (check-type alpha (or null (integer 0 255)))
  (claraoke:rgb (random 255) (random 255) (random 255) alpha))

