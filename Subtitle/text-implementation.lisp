(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text
;;;
(defmethod (setf claraoke:.text) :around (new-value (object dialogue))
  (flet ((textp (thing) (typep thing 'claraoke-text:text)))
    (let ((text (claraoke:.text object)))
      (cond ((and (textp text) (stringp new-value))
             (setf (claraoke:.text text) new-value))
            ((and (stringp text) (stringp new-value))
             (call-next-method (claraoke:text new-value) object))
            ((textp new-value)
             (call-next-method new-value object))
            (t (error 'claraoke:failed-to-create-text :object new-value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Overrides
;;;
(defmethod claraoke:overrides ((object dialogue))
  (claraoke:overrides (claraoke:.text object)))

(defmethod (setf claraoke:overrides) (new-value (object dialogue))
  (setf (claraoke:overrides (claraoke:.text object)) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dialogue
;;;
(defmethod claraoke:dialogue ((object claraoke-text:text) &rest initargs)
  (apply 'make-instance 'dialogue :allow-other-keys t :text object initargs))

(defmethod initialize-instance :after
    ((instance dialogue) &key generate-overrides-p spell-duration change-karaoke-type
                           keep-original-modifier-p remove-unknown-modifier-p
                           overrides)
  (let ((value (claraoke:.text instance)))
    (when (stringp value)
      (setf (claraoke:.text instance)
            (claraoke:text value :generate-overrides-p generate-overrides-p
                                 :keep-original-modifier-p keep-original-modifier-p
                                 :remove-unknown-modifier-p remove-unknown-modifier-p
                                 :spell-duration spell-duration
                                 :change-karaoke-type change-karaoke-type
                                 :overrides overrides)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert override
;;;
(defmethod claraoke:insert-override ((object dialogue) override)
  (claraoke:insert-override (claraoke:.text object) override)
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete override
;;;
(defmethod claraoke:delete-override ((object dialogue) override)
  (claraoke:delete-override (claraoke:.text object) override)
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find override
;;;
(defmethod claraoke:find-override ((object dialogue) override)
  (claraoke:find-override (claraoke:.text object) override))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort override
;;;
(defmethod claraoke:sort-overrides ((object dialogue))
  (claraoke:sort-overrides (claraoke:.text object))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert karaoke ( karaoke, karaoke-fill, karaoke-outline )
;;;
(defmethod claraoke:insert-karaoke ((object dialogue) index &optional value)
  (if (null value)
      (claraoke:insert-karaoke (claraoke:text object) index)
      (claraoke:insert-karaoke (claraoke:text object) index value))
  object)

(defmethod claraoke:insert-karaoke-fill ((object dialogue) index &optional value)
  (if (null value)
      (claraoke:insert-karaoke-fill (claraoke:text object) index)
      (claraoke:insert-karaoke-fill (claraoke:text object) index value))
  object)

(defmethod claraoke:insert-karaoke-outline ((object dialogue) index &optional value)
  (if (null value)
      (claraoke:insert-karaoke-outline (claraoke:text object) index)
      (claraoke:insert-karaoke-outline (claraoke:text object) index value))
  object)

