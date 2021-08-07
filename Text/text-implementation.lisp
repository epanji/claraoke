(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text
;;;
(defmethod claraoke:text ((object string))
  (make-instance 'text :text object))

(defmethod claraoke:text ((object null))
  (warn 'claraoke:null-object-warning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override
;;;
(defmethod claraoke:override ((position integer) (override-text string))
  (make-instance 'override :position position :text override-text))

(defmethod claraoke:override ((position integer) override-text)
  (claraoke:override position (write-to-string override-text)))

(defmethod claraoke:override (position override-text)
  (claraoke:override (claraoke-internal:integer-from-string position) override-text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override comparation (Internal)
;;;
(defun %compare-override (override1 override2 operator)
  (check-type override1 override)
  (check-type override2 override)
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
(defmethod claraoke:insert-override ((object text) (override override))
  (pushnew override (claraoke:overrides object) :test 'same-override-p)
  object)

(defmethod claraoke:insert-override ((object text) override)
  (error 'claraoke:object-must-be-override :object override))

(defmethod claraoke:insert-override (text override)
  (error 'claraoke:object-must-be-text :object text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete override
;;;
(defmethod claraoke:delete-override ((object text) (override override))
  (setf (claraoke:overrides object)
        (remove override (claraoke:overrides object)))
  object)

(defmethod claraoke:delete-override ((object text) override)
  (error 'claraoke:object-must-be-override :object override))

(defmethod claraoke:delete-override (text override)
  (error 'claraoke:object-must-be-text :object text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find override
;;;
(defmethod claraoke:find-override ((object list) (position integer))
  (find position object :key 'claraoke:position))

(defmethod claraoke:find-override ((object text) (position integer))
  (claraoke:find-override (claraoke:overrides object) position))

(defmethod claraoke:find-override ((object text) position)
  (error 'claraoke:object-must-be-integer :object position))

(defmethod claraoke:find-override (text position)
  (error 'claraoke:object-must-be-text :object text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort override
;;;
(defmethod claraoke:sort-overrides ((object text))
  (let ((overrides (claraoke:overrides object)))
    (setf (claraoke:overrides object)
          (sort overrides 'override-lessp))
    object))

(defmethod claraoke:sort-overrides ((object null))
  (warn 'claraoke:null-object-warning))

(defmethod claraoke:sort-overrides (text)
  (error 'claraoke:object-must-be-text :object text))

