(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text
;;;
(defmethod claraoke:text ((object string) &rest initargs)
  (apply 'make-instance 'text :allow-other-keys t :text object initargs))

(claraoke-internal:mimic-accessor claraoke:text (claraoke:.text object)
  (error 'claraoke:failed-to-create-text :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override
;;;
(defmethod claraoke:override ((override-string string) (index integer))
  (make-instance 'override :text override-string :index index ))

(defmethod claraoke:override ((override-string string) index)
  (claraoke:override override-string (claraoke-internal:integer-from-string index)))

(defmethod claraoke:override (override-string index)
  (claraoke:override (write-to-string override-string) index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override comparation (Internal)
;;;
(defun %compare-override (override1 override2 operator)
  (check-type override1 override)
  (check-type override2 override)
  (funcall operator
           (claraoke:index override1)
           (claraoke:index override2)))

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

(defmethod claraoke:insert-override (object override)
  (error 'claraoke:object-must-be-text :object object))

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

(defmethod claraoke:delete-override (object override)
  (error 'claraoke:object-must-be-text :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find override
;;;
(defmethod claraoke:find-override ((object list) (index integer))
  (find index object :key 'claraoke:index))

(defmethod claraoke:find-override ((object text) (index integer))
  (claraoke:find-override (claraoke:overrides object) index))

(defmethod claraoke:find-override ((object text) index)
  (error 'claraoke:object-must-be-integer :object index))

(defmethod claraoke:find-override (object index)
  (error 'claraoke:object-must-be-text :object object))

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

