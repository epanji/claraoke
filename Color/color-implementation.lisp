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

(defmethod claraoke:color ((object string))
  (cond ((html-color-p object)
         (html-color object))
        ((ass-color-p object)
         (ass-color object))
        (t (claraoke:color 0))))

(defmethod claraoke:color ((object (eql 0)))
  (claraoke:rgb 0 0 0 nil))

(defmethod claraoke:color ((object color))
  object)

(defmethod claraoke:color ((object null))
  (warn 'claraoke:null-object-warning))

(defmethod claraoke:color (color)
  (error 'claraoke:failed-to-create-color :object color))

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

(defmethod claraoke:colorstring ((object null))
  (warn 'claraoke:null-object-warning))

(defmethod claraoke:colorstring (color)
  (error 'claraoke:object-must-be-color :object color))

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
  (check-type alpha (or null (integer 0 255)))
  (claraoke:rgb (random 255) (random 255) (random 255) alpha))
