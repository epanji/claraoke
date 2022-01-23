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

(defvar *ignore-note-predicate* t)

(defvar *generate-overrides-predicate* nil)

(defvar *keep-original-modifier-predicate* nil)

(defvar *remove-unknown-modifier-predicate* nil)

(defvar *spell-duration* nil)

(defvar *change-karaoke-type* nil)

(declaim (type (or null subtitle) *subtitle*)
         (type (or null script-info styles events fonts graphics) *active-section*)
         (boolean *ignore-note-predicate* *generate-overrides-predicate*)
         (boolean *keep-original-modifier-predicate* *remove-unknown-modifier-predicate*)
         (type (or null unsigned-byte) *spell-duration*)
         (type (member nil :fill :outline) *change-karaoke-type*))

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

(defun split-line-values (string &rest keys)
  (let* ((pos (or (position #\: string) -1))
         (trimmed (string-trim '(#\Space #\Tab) (subseq string (1+ pos))))
         (keyargs (apply 'claraoke-internal:split-by-char-to-keys #\, trimmed keys))
         (final-keyargs (loop for (key arg) on keyargs by (function cddr)
                              unless (null arg)
                                collect key and collect arg)))
    (values final-keyargs (subseq string 0 (max pos 0)))))

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
         (<= 1 (length string))
         (char-equal #\; (elt string 0)))))

(defun section-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 1 (length string))
         (char-equal #\[ (elt string 0)))))

(defun header-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 7 (length string))
         (string-equal "Format:" (subseq string 0 7)))))

(defun style-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 6 (length string))
         (string-equal "Style:" (subseq string 0 6)))))

(defun dialogue-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 9 (length string))
         (string-equal "Dialogue:" (subseq string 0 9)))))

(defun comment-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 8 (length string))
         (string-equal "Comment:" (subseq string 0 8)))))

(defun picture-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 8 (length string))
         (string-equal "Picture:" (subseq string 0 8)))))

(defun sound-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 6 (length string))
         (string-equal "Sound:" (subseq string 0 6)))))

(defun movie-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 6 (length string))
         (string-equal "Movie:" (subseq string 0 6)))))

(defun command-line-p (string)
  (let ((string (remove-if 'unreadable-char-p string)))
    (and (stringp string)
         (<= 8 (length string))
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
(defun style-from-string (line)
  (check-type line string)
  (let* ((args (split-line-values line :name :fontname :fontsize :primary-colour
                                       :secondary-colour :outline-colour :back-colour :bold
                                       :italic :underline :strike-out :scale-x :scale-y :spacing
                                       :angle :border-style :outline :shadow
                                       :alignment :margin-l :margin-r :margin-v :encoding))
         (name (getf args :name "Default")))
    (remf args :name)
    (apply 'claraoke:style name args)))

(defun dialogue-from-string (line)
  (check-type line string)
  (let* ((args (split-line-values line :layer :start :end :style :name :margin-l
                                       :margin-r :margin-v :effect :text))
         (text (getf args :text ""))
         (final-args (list* :generate-overrides-p *generate-overrides-predicate*
                            :spell-duration *spell-duration*
                            :change-karaoke-type *change-karaoke-type*
                            :keep-original-modifier-p *keep-original-modifier-predicate*
                            :remove-unknown-modifier-p *remove-unknown-modifier-predicate*
                            args)))
    (remf final-args :text)
    (apply 'claraoke:dialogue text final-args)))

(defun comment-from-string (line)
  (check-type line string)
  (let* ((args (split-line-values line :layer :start :end :style :name :margin-l
                                       :margin-r :margin-v :effect :text))
         (text (getf args :text "")))
    (remf args :text)
    (apply 'claraoke:comment text args)))

(defun picture-from-string (line)
  (check-type line string)
  (let* ((args (split-line-values line :layer :start :end :style :name :margin-l
                                       :margin-r :margin-v :effect :text))
         (text (getf args :text "")))
    (remf args :text)
    (apply 'claraoke:picture text args)))

(defun sound-from-string (line)
  (check-type line string)
  (let* ((args (split-line-values line :layer :start :end :style :name :margin-l
                                       :margin-r :margin-v :effect :text))
         (text (getf args :text "")))
    (remf args :text)
    (apply 'claraoke:sound text args)))

(defun movie-from-string (line)
  (check-type line string)
  (let* ((args (split-line-values line :layer :start :end :style :name :margin-l
                                       :margin-r :margin-v :effect :text))
         (text (getf args :text "")))
    (remf args :text)
    (apply 'claraoke:movie text args)))

(defun command-from-string (line)
  (check-type line string)
  (let* ((args (split-line-values line :layer :start :end :style :name :margin-l
                                       :margin-r :margin-v :effect :text))
         (text (getf args :text "")))
    (remf args :text)
    (apply 'claraoke:command text args)))

(defun info-from-string (line)
  (check-type line string)
  (let* ((pos (or (position #\: line) -1))
         (descriptor (subseq line 0 (max pos 0)))
         (value (string-trim '(#\Space #\Tab) (subseq line (1+ pos)))))
    ;; Do not distinguish value between number and string
    (claraoke:info descriptor :value value)))

(defun note-from-string (line)
  (check-type line string)
  (let ((text (string-left-trim '(#\; #\! #\: #\Space #\Tab) line)))
    (claraoke:note text)))

(defun create-object-from-string (line)
  (when (null *subtitle*) (return-from create-object-from-string))
  (cond ((empty-line-p line) nil)       ; do nothing
        ((note-line-p line)
         (unless *ignore-note-predicate*
           (claraoke:insert-note *active-section* (note-from-string line))))
        ((section-line-p line)
         (let ((index (section-index line)))
           (if (null index)
               (when *ignore-note-predicate*
                 (setf *active-section* (claraoke:script-info index)))
               (setf *active-section* (aref (claraoke:lines *subtitle*) index)))))
        ((header-line-p line) nil)      ; do nothing
        ((style-line-p line)
         (claraoke:insert-style *active-section* (style-from-string line)))
        ((dialogue-line-p line)
         (claraoke:insert-event *active-section* (dialogue-from-string line)))
        ((comment-line-p line)
         (claraoke:insert-event *active-section* (comment-from-string line)))
        ((picture-line-p line)
         (claraoke:insert-event *active-section* (picture-from-string line)))
        ((sound-line-p line)
         (claraoke:insert-event *active-section* (sound-from-string line)))
        ((movie-line-p line)
         (claraoke:insert-event *active-section* (movie-from-string line)))
        ((command-line-p line)
         (claraoke:insert-event *active-section* (command-from-string line)))
        ((info-line-p line)
         ;; Info line could be script info, font and graphic
         (claraoke:insert-line *active-section* (info-from-string line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse Script
;;;
(defmethod claraoke:parse-script ((object pathname) &rest initargs)
  (with-open-file (stream object :direction :input)
    (apply 'claraoke:parse-script stream initargs)))

(defmethod claraoke:parse-script ((object string) &rest initargs)
  (apply 'claraoke:parse-script (make-string-input-stream object) initargs))

(defmethod claraoke:parse-script ((object stream) &rest initargs)
  (apply 'claraoke:parse-script (loop for line = (read-line object nil)
                                      until (null line)
                                      collect line)
         initargs))

(defmethod claraoke:parse-script
    ((object cons)
     &key (ignore-note-p t) generate-overrides-p spell-duration change-karaoke-type
       keep-original-modifier-p remove-unknown-modifier-p)
  (let ((*subtitle* (claraoke:subtitle nil))
        (*active-section* nil)
        (*ignore-note-predicate* ignore-note-p)
        (*generate-overrides-predicate* generate-overrides-p)
        (*keep-original-modifier-predicate* keep-original-modifier-p)
        (*remove-unknown-modifier-predicate* remove-unknown-modifier-p)
        (*spell-duration* spell-duration)
        (*change-karaoke-type* change-karaoke-type)
        (char-bag (list #\Newline #\Return #\Page #\Linefeed)))
    (loop for line in object
          do (create-object-from-string (string-trim char-bag line))
          finally (return *subtitle*))))

