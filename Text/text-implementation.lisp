(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text
;;;
(defmethod claraoke:text ((object string) &rest initargs)
  (apply 'make-instance 'text :allow-other-keys t :original-text object initargs))

(claraoke-internal:mimic-accessor claraoke:text (claraoke:.text object)
  (error 'claraoke:failed-to-create-text :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override
;;;
(defmethod claraoke:override ((override symbol) (index integer) &rest initargs)
  (let ((osymbol (find-symbol (symbol-name override) :claraoke-text)))
    (if (typep (make-instance osymbol) 'override)
        (apply 'make-instance osymbol :index index initargs)
        (apply 'make-instance 'batch
               :allow-other-keys t
               :index index
               :overrides (list (apply 'make-instance osymbol
                                       :allow-other-keys t initargs))
               initargs))))

(defmethod claraoke:override ((override symbol) (index null) &rest initargs)
  (let ((osymbol (find-symbol (symbol-name override) :claraoke-text)))
    (apply 'make-instance osymbol :allow-other-keys t initargs)))

(defmethod claraoke:override ((override symbol) index &rest initargs)
  (let ((index (claraoke-internal:integer-from-string index)))
    (apply 'claraoke:override override :allow-other-keys t index initargs)))

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

(defmethod claraoke:insert-override ((object batch) (override modifier))
  (pushnew override (claraoke:overrides object) :key 'type-of)
  object)

(defmethod claraoke:insert-override ((object batch) (override unknown))
  (push override (claraoke:overrides object))
  object)

(defmethod claraoke:insert-override (object override)
  (error 'claraoke:object-must-be-text :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete override
;;;
(defmethod claraoke:delete-override ((object text) (override override))
  (claraoke-internal:deletef override (claraoke:overrides object))
  object)

(defmethod claraoke:delete-override ((object batch) (override modifier))
  (claraoke-internal:deletef override (claraoke:overrides object))
  object)

(defmethod claraoke:delete-override ((object text) (override null))
  (warn 'claraoke:null-object-warning))

(defmethod claraoke:delete-override ((object text) override)
  (error 'claraoke:object-must-be-override :object override))

(defmethod claraoke:delete-override (object override)
  (error 'claraoke:object-must-be-text :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find override
;;;
(defmethod claraoke:find-override ((object list) (index integer))
  (find index object :from-end t :key 'claraoke:index))

(defmethod claraoke:find-override ((object text) (index integer))
  (claraoke:find-override (claraoke:overrides object) index))

(defmethod claraoke:find-override ((object text) (index string))
  (let ((index (search index (claraoke:.text object))))
    (or (claraoke:find-override object index)
        (claraoke:find-override object (1- index))
        (claraoke:find-override object (1+ index)))))

(defmethod claraoke:find-override ((object text) index)
  (error 'claraoke:object-must-be-integer :object index))

(defmethod claraoke:find-override ((object batch) (name symbol))
  (let ((osymbol (find-symbol (symbol-name name) :claraoke-text)))
    (find osymbol (claraoke:overrides object) :key 'type-of)))

(defmethod claraoke:find-override (object index)
  (error 'claraoke:object-must-be-text :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Increase override
;;;
(defmethod claraoke:increase-override ((object override) &optional (delta 1))
  (incf (claraoke:index object) delta))

(defmethod claraoke:increase-override ((object null) &optional delta)
  (declare (ignore delta))
  object)

(defmethod claraoke:increase-override (object &optional delta)
  (declare (ignore delta))
  (error 'claraoke:object-must-be-override :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrease override
;;;
(defmethod claraoke:decrease-override ((object override) &optional (delta 1))
  (decf (claraoke:index object) delta))

(defmethod claraoke:decrease-override ((object null) &optional delta)
  (declare (ignore delta))
  object)

(defmethod claraoke:decrease-override (object &optional delta)
  (declare (ignore delta))
  (error 'claraoke:object-must-be-override :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort override
;;;
(defmethod claraoke:sort-overrides ((object text))
  (sort (claraoke:overrides object) 'override-lessp))

(defmethod claraoke:sort-overrides ((object null))
  (warn 'claraoke:null-object-warning))

(defmethod claraoke:sort-overrides (text)
  (error 'claraoke:object-must-be-text :object text))

