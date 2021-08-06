(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print text
;;;
(defmethod claraoke:print-text ((object claraoke:text) &optional stream)
  (let ((overrides (claraoke:overrides object))
        (string (claraoke:text object))
        (stream (claraoke-internal:output-stream-from-designator stream)))
    (loop for position from 0
          for char across string
          and override = (claraoke:find-override overrides position)
          do (print-override override stream)
             (princ char stream))
    (values object)))

(defmethod claraoke:print-text ((object null) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (values object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print override (Internal)
;;;
(defmethod print-override ((object claraoke:override) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (princ #\{ stream)
    (print-override (claraoke:text object) stream)
    (princ #\} stream)
    (values object)))

(defmethod print-override ((object string) &optional stream)
  ;; Function split-sequence always return CONS or NULL
  (let ((stream (claraoke-internal:output-stream-from-designator stream))
        (strings (split-sequence:split-sequence #\; object :remove-empty-subseqs t)))
    (print-override strings stream)
    (values object)))

(defmethod print-override ((object list) &optional stream)
  ;; LISTP for CONS and NULL is T
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (loop for string in object
          do (princ #\\ stream)
             (princ string stream))
    (values object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override comparation (Internal)
;;;
(defun %compare-override (override1 override2 operator)
  (check-type override1 claraoke:override)
  (check-type override2 claraoke:override)
  (funcall operator
           (claraoke:position override1)
           (claraoke:position override2)))

(defun same-override-p (override1 override2)
  (%compare-override override1 override2 '=))

(defun override-lessp (override1 override2)
  (%compare-override override1 override2 '<))

(defun override-greaterp (override1 override2)
  (%compare-override override1 override2 '>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert override
;;;
(defmethod claraoke:insert-override ((text claraoke:text) (override claraoke:override))
  (pushnew override (claraoke:overrides text) :test 'same-override-p))

(defmethod claraoke:insert-override (text override)
  (error 'claraoke:object-must-be-text :object text))

(defmethod claraoke:insert-override ((text claraoke:text) override)
  (error 'claraoke:object-must-be-override :object override))

(defmethod print-object ((text claraoke:text) stream)
  (princ (claraoke:text text) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete override
;;;
(defmethod claraoke:delete-override ((text claraoke:text) (override claraoke:override))
  (setf (claraoke:overrides text)
        (remove override (claraoke:overrides text))))

(defmethod claraoke:delete-override (text override)
  (error 'claraoke:object-must-be-text :object text))

(defmethod claraoke:delete-override ((text claraoke:text) override)
  (error 'claraoke:object-must-be-override :object override))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find override
;;;
(defmethod claraoke:find-override ((text claraoke:text) (position integer))
  (claraoke:find-override (claraoke:overrides text) position))

(defmethod claraoke:find-override ((overrides list) (position integer))
  (find position overrides :key 'claraoke:position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort override
;;;
(defmethod claraoke:sort-overrides ((text claraoke:text))
  (let ((overrides (claraoke:overrides text)))
    (setf (claraoke:overrides text)
          (sort overrides 'override-lessp))))

