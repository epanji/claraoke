(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialization after
;;;
(defmethod initialize-instance :after ((object text) &key original-text generate-overrides-p)
  (unless (null original-text)
    (when generate-overrides-p
      (setf original-text (defile-text original-text)))
    (multiple-value-bind (string overrides) (purify-text original-text)
      (setf (claraoke:text object) string)
      (setf (claraoke:overrides object) overrides))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser
;;;
(defvar *string* nil)
(defvar *length* nil)
(defvar *index* nil)
(defvar *text-index* nil)

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
(defun start-override-matcher ()
  (when (valid-index-p)
    (let ((char1 (peek))
          (char2 (peek 1)))
      (case char1
        ((or nil #\{) t)
        (#\\ (or (char= #\n char2)
                 (char= #\N char2)))))))

(defun end-override-matcher ()
  (when (valid-index-p)
    (let ((char1 (peek))
          (char2 (peek -1)))
      (case char1
        (#\} t)
        ((or #\n #\N) (char= #\\ char2))
        (t (null char1))))))

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

(defun normalize-override-text (string)
  (with-output-to-string (out)
    (loop for c across (string-trim "{\\}" string)
          do (if (char= #\\ c)
                 (princ #\; out)
                 (princ c out)))))

(defun build-string-or-override ()
  (if (start-override-matcher)
      (let* ((text1 (consume-override))
             (text2 (normalize-override-text text1)))
        (make-instance 'override :index *text-index* :text text2))
      (let ((text1 (consume-text)))
        (incf *text-index* (length text1))
        (normalize-override-text text1))))

(defun purify-text (string)
  (let ((*string* string)
        (*length* (length string))
        (*index* 0)
        (*text-index* 0))
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
(defvar *vowel* "AEIOUYaeiouy")
(defvar *consonant* "BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz")
(defvar *strong-consonant* "BCDFJKMNPQSTVWXZbcdfjkmnpqstvwxz")
(defvar *weak-consonant* "GHLRghlr")
(defvar *estimate-karaoke-duration* 15)

(defun end-spelling-matcher ()
  (let ((char0 (peek 0))
        (char1 (peek 1))
        (char2 (peek 2))
        (char3 (peek 3))
        (char-1 (peek -1)))
    (and (not (null char0))
         (or (char= #\Space char0)
             ;; Check vowel
             (and (find char0 *vowel*)
                  (find char1 *consonant*)
                  (or (find char2 *vowel*)
                      (and (find char2 *weak-consonant*)
                           (find char3 *vowel*))))
             ;; Check consonant
             (and (find char0 *consonant*)
                  (find char1 *strong-consonant*)
                  (or (find char2 *vowel*)
                      (find char2 *weak-consonant*))
                  (or (find char-1 *vowel*)
                      (find char-1 *weak-consonant*)))))))

(defun consume-spelling ()
  (loop with start = *index*
        until (end-spelling-matcher)
        while (consume)
        finally (consume)
                (return (subseq *string* start *index*))))

(defun compute-override ()
  (let ((text (consume-spelling)))
    (format nil "{\\k~D}~A"
            (max *estimate-karaoke-duration*
                 (* (loop for c across *vowel*
                          sum (count c text))
                    *estimate-karaoke-duration*))
            text)))

(defun defile-text (string)
  (let ((*string* string)
        (*length* (length string))
        (*index* 0)
        (*text-index* 0))
    (if (zerop (count #\\ string))
        (loop while (peek)
              collect (compute-override) into list-strings
              finally (return (apply 'concatenate 'string list-strings)))
        string)))

