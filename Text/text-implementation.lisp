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
(defun symbol-from-string (name &optional (package :claraoke-text))
  (check-type name string)
  (let ((package (find-package package))
        (case-name (case *print-case*
                     (:upcase (string-upcase name))
                     (:downcase (string-downcase name))
                     (otherwise name))))
    (find-symbol case-name package)))

(defmethod claraoke:override ((override string) (index integer) &rest initargs)
  (let ((symbol (symbol-from-string override :claraoke-text))
        (modifiers (getf initargs :modifiers '())))
    (if (subtypep symbol 'override)
        (apply 'make-instance symbol
               :index index
               :modifiers (reverse modifiers)
               :allow-other-keys t
               initargs)
        (apply 'make-instance 'batch
               :index index
               :modifiers (reverse (list* (apply 'make-instance symbol
                                                 :allow-other-keys t initargs)
                                          modifiers))
               :allow-other-keys t
               initargs))))

(defmethod claraoke:override ((override string) (index null) &rest initargs)
  (apply 'claraoke:modifier override :allow-other-keys t initargs))

(defmethod claraoke:override ((override string) index &rest initargs)
  (let ((index (claraoke-internal:integer-from-string index)))
    (apply 'claraoke:override override index :allow-other-keys t initargs)))

(defmethod claraoke:override ((override symbol) index &rest initargs)
  (let ((override (symbol-name override)))
    (apply 'claraoke:override override index :allow-other-keys t initargs)))

(defmethod claraoke:override (override index &rest initargs)
  (let ((override (write-to-string override)))
    (apply 'claraoke:override override index :allow-other-keys t initargs)))

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
  (claraoke-internal:deletef override (claraoke:overrides object))
  object)

(defmethod claraoke:delete-override ((object text) (override string))
  (claraoke:delete-override object (claraoke:find-override object override)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifier
;;;
(defmethod claraoke:modifier ((object string) &rest initargs)
  (let ((symbol (symbol-from-string object :claraoke-text)))
    (when (subtypep symbol 'modifier)
      (apply 'make-instance symbol :allow-other-keys t initargs))))

(defmethod claraoke:modifier ((object symbol) &rest initargs)
  (apply 'claraoke:modifier (symbol-name object) :allow-other-keys t initargs))

(defmethod claraoke:modifier (object &key)
  (apply 'claraoke:modifier (write-to-string object) :allow-other-keys t initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert modifier
;;;
(defmethod claraoke:insert-modifier ((object batch) (modifier modifier))
  (pushnew modifier (claraoke:modifiers object) :key 'type-of)
  object)

(defmethod claraoke:insert-modifier ((object batch) (modifier unknown))
  (push modifier (claraoke:modifiers object))
  object)

(defmethod claraoke:insert-modifier ((object batch) modifier)
  (claraoke:insert-modifier object (claraoke:modifier modifier)))

(defmethod claraoke:insert-modifier (object modifier)
  (error 'claraoke:object-must-be-batch :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete modifier
;;;
(defmethod claraoke:delete-override ((object batch) (modifier modifier))
  (claraoke-internal:deletef modifier (claraoke:modifiers object))
  object)

(defmethod claraoke:delete-modifier ((object batch) (modifier string))
  (claraoke:delete-modifier object (claraoke:find-modifier object modifier)))

(defmethod claraoke:delete-modifier ((object batch) (modifier null))
  (warn 'claraoke:null-object-warning))

(defmethod claraoke:delete-modifier ((object batch) modifier)
  (error 'claraoke:object-must-be-modifier :object modifier))

(defmethod claraoke:delete-modifier (object modifier)
  (error 'claraoke:object-must-be-batch :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find modifier
;;;
(defmethod claraoke:find-modifier ((object batch) (modifier string))
  (let ((symbol (symbol-from-string modifier :claraoke-text)))
    (find symbol (claraoke:modifiers object) :key 'type-of)))

(defmethod claraoke:find-modifier ((object batch) (modifier symbol))
  (claraoke:find-modifier object (symbol-name modifier)))

(defmethod claraoke:find-modifier ((object batch) modifier)
  (claraoke:find-modifier object (write-to-string modifier)))

(defmethod claraoke:find-modifier (object modifier)
  (error 'claraoke:object-must-be-batch :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Increase modifier
;;;
(defmethod claraoke:increase-modifier ((object modifier) &optional (delta 1) (key :arg1))
  (ecase key
    (:arg1 (incf (claraoke:arg1 object) delta))
    (:arg2 (incf (claraoke:arg2 object) delta))
    (:arg3 (incf (claraoke:arg3 object) delta))
    (:arg4 (incf (claraoke:arg4 object) delta))
    (:arg5 (incf (claraoke:arg5 object) delta))
    (:arg6 (incf (claraoke:arg6 object) delta))
    (:arg7 (incf (claraoke:arg7 object) delta)))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrease modifier
;;;
(defmethod claraoke:decrease-modifier ((object modifier) &optional (delta 1) (key :arg1))
  (ecase key
    (:arg1 (decf (claraoke:arg1 object) delta))
    (:arg2 (decf (claraoke:arg2 object) delta))
    (:arg3 (decf (claraoke:arg3 object) delta))
    (:arg4 (decf (claraoke:arg4 object) delta))
    (:arg5 (decf (claraoke:arg5 object) delta))
    (:arg6 (decf (claraoke:arg6 object) delta))
    (:arg7 (decf (claraoke:arg7 object) delta)))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update modifier
;;;
(defmethod claraoke:update-modifier ((object modifier) &optional (delta 1) (key :arg1))
  (ecase key
    (:arg1 (setf (claraoke:arg1 object) delta))
    (:arg2 (setf (claraoke:arg2 object) delta))
    (:arg3 (setf (claraoke:arg3 object) delta))
    (:arg4 (setf (claraoke:arg4 object) delta))
    (:arg5 (setf (claraoke:arg5 object) delta))
    (:arg6 (setf (claraoke:arg6 object) delta))
    (:arg7 (setf (claraoke:arg7 object) delta)))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Increase karaoke
;;;
(defmethod claraoke:increase-karaoke
    ((object karaoke) &optional (delta *spell-duration-in-centiseconds*))
  (incf (claraoke:arg1 object) delta)
  object)

(defmethod claraoke:increase-karaoke
    ((object karaoke-fill) &optional (delta *spell-duration-in-centiseconds*))
  (incf (claraoke:arg1 object) delta)
  object)

(defmethod claraoke:increase-karaoke
    ((object karaoke-outline) &optional (delta *spell-duration-in-centiseconds*))
  (incf (claraoke:arg1 object) delta)
  object)

(defmethod claraoke:increase-karaoke
    ((object batch) &optional (delta *spell-duration-in-centiseconds*))
  (claraoke:increase-karaoke (or (claraoke:find-modifier object :karaoke)
                                 (claraoke:find-modifier object :karaoke-fill)
                                 (claraoke:find-modifier object :karaoke-outline))
                             delta))

(defmethod claraoke:increase-karaoke ((object null) &optional delta)
  (declare (ignore delta))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrease karaoke
;;;
(defmethod claraoke:decrease-karaoke
    ((object karaoke) &optional (delta *spell-duration-in-centiseconds*))
  (decf (claraoke:arg1 object) delta)
  object)

(defmethod claraoke:decrease-karaoke
    ((object karaoke-fill) &optional (delta *spell-duration-in-centiseconds*))
  (decf (claraoke:arg1 object) delta)
  object)

(defmethod claraoke:decrease-karaoke
    ((object karaoke-outline) &optional (delta *spell-duration-in-centiseconds*))
  (decf (claraoke:arg1 object) delta)
  object)

(defmethod claraoke:decrease-karaoke
    ((object batch) &optional (delta *spell-duration-in-centiseconds*))
  (claraoke:decrease-karaoke (or (claraoke:find-modifier object :karaoke)
                                 (claraoke:find-modifier object :karaoke-fill)
                                 (claraoke:find-modifier object :karaoke-outline))
                             delta))

(defmethod claraoke:decrease-karaoke ((object null) &optional delta)
  (declare (ignore delta))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update karaoke
;;;
(defmethod claraoke:update-karaoke ((object karaoke) (value integer))
  (setf (claraoke:arg1 object) value)
  object)

(defmethod claraoke:update-karaoke ((object karaoke-fill) (value integer))
  (setf (claraoke:arg1 object) value)
  object)

(defmethod claraoke:update-karaoke ((object karaoke-outline) (value integer))
  (setf (claraoke:arg1 object) value)
  object)

(defmethod claraoke:update-karaoke ((object batch) (value integer))
  (claraoke:update-karaoke (or (claraoke:find-modifier object :karaoke)
                               (claraoke:find-modifier object :karaoke-fill)
                               (claraoke:find-modifier object :karaoke-outline))
                           value))

(defmethod claraoke:update-karaoke ((object null) value)
  object)

