(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;
(defvar *subtitle* nil)

(defvar *active-section* nil)

(defvar *section-index*
  '(("[Script Info]" 0)
    ("[V4+ Styles]" 1)
    ("[V4 Styles]" 1)
    ("[Styles]" 1)
    ("[Events]" 2)
    ("[Fonts]" 3)
    ("[Graphics]" 4)))

(defparameter *ignore-note-predicate* t)

(declaim (type (or null subtitle) *subtitle*)
         (type (or null script-info styles events fonts graphics) *active-section*)
         (boolean *ignore-note-predicate*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions
;;;
(defun unreadable-char-p (char)
  (or (< (char-int char) 32)
      (> (char-int char) 126)))

(defun section-index (string)
  (let ((item (remove-if 'unreadable-char-p string)))
    (second (assoc item *section-index* :test 'string-equal))))

(defun split-line-values (string &optional (column 1))
  (let ((pos (1+ (or (position #\: string) -1))))
    (values (loop with value = (string-trim '(#\Space #\Tab) (subseq string pos))
                  and pos1 = 0
                  and max = (1- column)
                  for pos2 = (if (plusp max)
                                 (+ pos1 (or (position #\, (subseq value pos1)) 0))
                                 (length value))
                  until (minusp max)
                  collect (prog1 (subseq value pos1 pos2)
                            (setf pos1 (1+ pos2))
                            (decf max)))
            (subseq string 0 (max (1- pos) 0)))))

(defun claraoke:ignore-note (&optional (arg :peek))
  (ecase arg
    ((or nil t) (setf *ignore-note-predicate* arg))
    (:toggle (setf *ignore-note-predicate* (not *ignore-note-predicate*)))
    (:peek *ignore-note-predicate*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Predicate functions
;;;
(defun empty-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (zerop (length string)))))

(defun note-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (char-equal #\; (elt string 0)))))

(defun section-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (char-equal #\[ (elt string 0)))))

(defun header-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (string-equal "Format:" (subseq string 0 7)))))

(defun style-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (string-equal "Style:" (subseq string 0 6)))))

(defun dialogue-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (string-equal "Dialogue:" (subseq string 0 9)))))

(defun comment-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (string-equal "Comment:" (subseq string 0 8)))))

(defun picture-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (string-equal "Picture:" (subseq string 0 8)))))

(defun sound-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (string-equal "Sound:" (subseq string 0 6)))))

(defun movie-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (string-equal "Movie:" (subseq string 0 6)))))

(defun command-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (string-equal "Command:" (subseq string 0 8)))))

(defun info-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (find #\: string)
         (>= 8 (count #\, string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object from line string function
;;;
(defun create-object-from-string (line)
  (when (null *subtitle*) (return-from create-object-from-string))
  (cond ((empty-line-p line)
         ())
        ((note-line-p line)
         (unless *ignore-note-predicate*
           (claraoke:insert-note *active-section* (claraoke:note (subseq line 2)))))
        ((section-line-p line)
         (let ((index (section-index line)))
           (if (null index)
               (when *ignore-note-predicate*
                 (setf *active-section* (claraoke:script-info index)))
               (setf *active-section* (aref (claraoke:lines *subtitle*) index)))))
        ((header-line-p line)
         ())
        ((style-line-p line)
         (destructuring-bind (name fontname fontsize
                              primary-colour secondary-colour outline-colour back-colour
                              bold italic underline strike-out
                              scale-x scale-y spacing angle
                              border-style outline shadow alignment
                              margin-l margin-r margin-v encoding)
             (split-line-values line 23)
           (claraoke:insert-style
            *active-section*
            (claraoke:style name :fontname fontname
                                 :fontsize fontsize
                                 :primary-colour primary-colour
                                 :secondary-colour secondary-colour
                                 :outline-colour outline-colour
                                 :back-colour back-colour
                                 :bold bold
                                 :italic italic
                                 :underline underline
                                 :strike-out strike-out
                                 :scale-x scale-x
                                 :scale-y scale-y
                                 :spacing spacing
                                 :angle angle
                                 :border-style border-style
                                 :outline outline
                                 :shadow shadow
                                 :alignment alignment
                                 :margin-l margin-l
                                 :margin-r margin-r
                                 :margin-v margin-v
                                 :encoding encoding))))
        ((dialogue-line-p line)
         (destructuring-bind (layer start end style name margin-l margin-r margin-v effect text)
             (split-line-values line 10)
           (claraoke:insert-event
            *active-section*
            (claraoke:dialogue text :layer layer
                                    :start start
                                    :end end
                                    :style style
                                    :name name
                                    :margin-l margin-l
                                    :margin-r margin-r
                                    :margin-v margin-v
                                    :effect effect))))
        ((comment-line-p line)
         (destructuring-bind (layer start end style name margin-l margin-r margin-v effect text)
             (split-line-values line 10)
           (claraoke:insert-event
            *active-section*
            (claraoke:comment text :layer layer
                                   :start start
                                   :end end
                                   :style style
                                   :name name
                                   :margin-l margin-l
                                   :margin-r margin-r
                                   :margin-v margin-v
                                   :effect effect))))
        ((picture-line-p line)
         (destructuring-bind (layer start end style name margin-l margin-r margin-v effect text)
             (split-line-values line 10)
           (claraoke:insert-event
            *active-section*
            (claraoke:picture text :layer layer
                                   :start start
                                   :end end
                                   :style style
                                   :name name
                                   :margin-l margin-l
                                   :margin-r margin-r
                                   :margin-v margin-v
                                   :effect effect))))
        ((sound-line-p line)
         (destructuring-bind (layer start end style name margin-l margin-r margin-v effect text)
             (split-line-values line 10)
           (claraoke:insert-event
            *active-section*
            (claraoke:sound text :layer layer
                                 :start start
                                 :end end
                                 :style style
                                 :name name
                                 :margin-l margin-l
                                 :margin-r margin-r
                                 :margin-v margin-v
                                 :effect effect))))
        ((movie-line-p line)
         (destructuring-bind (layer start end style name margin-l margin-r margin-v effect text)
             (split-line-values line 10)
           (claraoke:insert-event
            *active-section*
            (claraoke:movie text :layer layer
                                 :start start
                                 :end end
                                 :style style
                                 :name name
                                 :margin-l margin-l
                                 :margin-r margin-r
                                 :margin-v margin-v
                                 :effect effect))))
        ((command-line-p line)
         (destructuring-bind (layer start end style name margin-l margin-r margin-v effect text)
             (split-line-values line 10)
           (claraoke:insert-event
            *active-section*
            (claraoke:command text :layer layer
                                   :start start
                                   :end end
                                   :style style
                                   :name name
                                   :margin-l margin-l
                                   :margin-r margin-r
                                   :margin-v margin-v
                                   :effect effect))))
        ((info-line-p line)
         (multiple-value-bind (line-values descriptor) (split-line-values line)
           (claraoke:insert-info
            *active-section*
            (claraoke:info descriptor :value (first line-values)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse Script
;;;
(defmethod claraoke:parse-script ((object pathname))
  (with-open-file (stream object :direction :input)
    (claraoke:parse-script stream)))

(defmethod claraoke:parse-script ((object string))
  (claraoke:parse-script (make-string-input-stream object)))

(defmethod claraoke:parse-script ((object stream))
  (claraoke:parse-script (loop for line = (read-line object nil)
                               until (null line)
                               collect line)))

(defmethod claraoke:parse-script ((object cons))
  (let ((*subtitle* (claraoke:subtitle nil))
        (*active-section* nil)
        (char-bag (list #\Newline #\Return #\Page #\Linefeed)))
    (loop for line in object
          do (create-object-from-string (string-trim char-bag line))
          finally (return *subtitle*))))

