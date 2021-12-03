(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialization after
;;;
(defmethod initialize-instance :after
    ((object text) &key original-text generate-overrides-p spell-duration change-karaoke-type
                     keep-original-modifier-p remove-unknown-modifier-p)
  (let ((original-text (or original-text (claraoke:original-text object))))
    (unless (null original-text)
      (when generate-overrides-p
        (setf original-text (defile-text original-text spell-duration change-karaoke-type)))
      (multiple-value-bind (string overrides)
          (purify-text original-text keep-original-modifier-p remove-unknown-modifier-p)
        (setf (claraoke:.text object) string)
        (when (null (claraoke:overrides object))
          (setf (claraoke:overrides object) overrides))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser
;;;
(defvar *string* nil)
(defvar *length* nil)
(defvar *index* nil)
(defvar *text-index* nil)

(declaim (type (or null string) *string*)
         (type (or null unsigned-byte) *length* *index* *text-index*))

(defun advance (&optional (n 1))
  (incf *index* n))

(defun valid-index-p (&optional (delta 0))
  (let ((index (+ *index* delta)))
    (< -1 index *length*)))

(defun peek (&optional (n 0))
  (when (valid-index-p n)
    (aref *string* (+ *index* n))))

(defun consume (&optional (n 1))
  (when (valid-index-p)
    (prog1 (peek (1- n))
      (advance n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override Text Builder
;;;
(defvar *keep-original-modifier-predicate* nil
  "Keep default modifier instead changing to most specific modifier.
For example, modifier \\alpha&H00& will not be changed to \\1a&H00&.")

(defvar *remove-unknown-modifier-predicate* nil
  "Remove unknown modifier from override.")

(defvar *batch-predicate* nil
  "Prevent modifiers in batch being parsed as newline.
For example, unknown modifier {\\note} will not be parsed as newline.")

(declaim (boolean *keep-original-modifier-predicate*
                  *remove-unknown-modifier-predicate*
                  *batch-predicate*))

(defun start-override-matcher ()
  (when (valid-index-p)
    (let ((char0 (peek 0))
          (char1 (peek 1))
          (char-1 (peek -1)))
      (case char0
        ((nil #\{) (case char-1
                     (#\} nil)
                     (t (setf *batch-predicate* t) t)))
        (#\\ (or (char= #\n char1)
                 (char= #\N char1)))))))

(defun end-override-matcher ()
  (when (valid-index-p)
    (let ((char0 (peek 0))
          (char1 (peek 1))
          (char-1 (peek -1)))
      (case char0
        (#\} (case char1
               (#\{ nil)
               (t (setf *batch-predicate* nil) t)))
        ((#\n #\N) (and (char= #\\ char-1)
                        (not *batch-predicate*)))
        (t (null char0))))))

(defun consume-override ()
  (loop with start = *index*
        until (end-override-matcher)
        while (consume)
        finally (consume)
                (return (subseq *string* start *index*))))

(defun consume-text ()
  (loop with start = *index*
        until (start-override-matcher)
        while (consume)
        finally (return (subseq *string* start *index*))))

(defun build-string-or-override ()
  (let ((*batch-predicate* nil))
    (if (start-override-matcher)
        (let ((text (consume-override)))
          (override-from-string text *text-index*))
        (let ((text (consume-text)))
          (incf *text-index* (length text))
          text))))

(defun purify-text (string &optional keep-original-modifier-p remove-unknown-modifier-p)
  (let ((*string* string)
        (*length* (length string))
        (*index* 0)
        (*text-index* 0)
        (*keep-original-modifier-predicate* keep-original-modifier-p)
        (*remove-unknown-modifier-predicate* remove-unknown-modifier-p))
    (loop while (peek)
          for object = (build-string-or-override)
          if (stringp object)
            collect object into list-strings
          else
            collect object into overrides
          finally (let ((text (apply 'concatenate 'string list-strings)))
                    (return (values text overrides))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override Spell Builder
;;;
(defvar *vowels* "AEIOUYaeiouy")

(defvar *consonants* "BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz")

(defvar *weak-consonants* "GHLRghlr"
  "Every consonant characters which have vowels behaviour such as:
G in RANGE, H in THE, L in FLOW and R in WRITE.")

(defvar *strong-consonants* "BCDFJKMNPQSTVWXZbcdfjkmnpqstvwxz"
  "Every consonant characters without weak consonants.")

(defvar *spell-duration-in-centiseconds* 15
  "Estimate karaoke duration in centiseconds.")

(defparameter *change-karaoke-type* nil
  "Change karaoke type to either :FILL or :OUTLINE, otherwise it will use default type.")

(declaim (string *vowels* *consonants* *weak-consonants* *strong-consonants*)
         (unsigned-byte *spell-duration-in-centiseconds*)
         (type (member nil :fill :outline) *change-karaoke-type*))

(defun end-spelling-matcher ()
  (let ((char0 (peek 0))
        (char1 (peek 1))
        (char2 (peek 2))
        (char3 (peek 3))
        (char-1 (peek -1))
        (separators (list #\Space #\-)))
    (and (characterp char0)
         (or (member char0 separators)
             ;; Check non-ASCII
             (not (standard-char-p char0))
             ;; Check vowels
             (and (find char0 *vowels*)
                  (find char-1 *consonants*)
                  (or (and (find char1 *weak-consonants*)
                           (find char2 *vowels*))
                      (and (find char1 *strong-consonants*)
                           (or (find char2 *vowels*)
                               (and (find char2 *weak-consonants*)
                                    (find char3 *vowels*))))))
             ;; Check consonants
             (and (not (member char-1 separators))
                  (or (and (equal char0 char1)
                           (find char2 *vowels*))
                      (and (find char0 *strong-consonants*)
                           (find char1 *strong-consonants*)
                           (find char2 *vowels*))
                      (and (find char0 *strong-consonants*)
                           (find char1 *weak-consonants*)
                           (find char2 *vowels*)
                           (find char-1 *consonants*))
                      (and (find char0 *weak-consonants*)
                           (find char1 *consonants*)
                           (find char2 *vowels*)))))
         char0)))

(defun consume-spelling ()
  (loop with start = *index*
        until (end-spelling-matcher)
        while (consume)
        finally (consume)
                (return (subseq *string* start *index*))))

(defun compute-override ()
  (let* ((text (consume-spelling))
         (len (length (remove-if (complement (function alpha-char-p)) text)))
         (sum (loop for char across *vowels*
                    sum (count char text))))
    (format nil "{\\~A~D}~A"
            (case *change-karaoke-type*
              (:fill "kf")
              (:outline "ko")
              (otherwise "k"))
            (max *spell-duration-in-centiseconds*
                 (* (min (- len sum) sum) *spell-duration-in-centiseconds*))
            text)))

(defun defile-text (string &optional spell-duration change-karaoke-type)
  (when (null spell-duration)
    (setf spell-duration *spell-duration-in-centiseconds*))
  (let ((*string* string)
        (*length* (length string))
        (*index* 0)
        (*text-index* 0)
        (*spell-duration-in-centiseconds* spell-duration)
        (*change-karaoke-type* change-karaoke-type))
    (if (zerop (count #\\ string))
        (loop while (peek)
              collect (compute-override) into list-strings
              finally (return (apply 'concatenate 'string list-strings)))
        string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override Modifiers Splitter
;;;
(defvar *char-inside-parenthesis-predicate* nil)

(declaim (boolean *char-inside-parenthesis-predicate*))

(defun end-modifier-matcher ()
  (let ((char0 (peek 0)))
    (when (equal #\( char0) (setf *char-inside-parenthesis-predicate* t))
    (when (equal #\) char0) (setf *char-inside-parenthesis-predicate* nil))
    (unless *char-inside-parenthesis-predicate* (equal #\\ char0))))

(defun consume-modifier ()
  (loop with start = *index*
        until (end-modifier-matcher)
        while (consume)
        finally (return
                  (prog1 (subseq *string* start *index*)
                    (consume)))))

(defun split-modifier (string)
  (setf string (remove-if
                (lambda (c)
                  (or (char= #\} c)
                      (char= #\{ c)))
                (string-trim "\\" string)))
  (let ((*string* string)
        (*length* (length string))
        (*index* 0)
        (*char-inside-parenthesis-predicate* nil))
    (if (plusp (count #\\ string))
        (loop while (peek)
              for modifier = (consume-modifier)
              when (plusp (length modifier))
                collect modifier)
        (list string))))

