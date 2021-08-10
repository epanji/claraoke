(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors
;;;
(defmethod claraoke:overrides ((object dialogue))
  (claraoke:overrides (claraoke:.text object)))

(defmethod (setf claraoke:overrides) (new-value (object dialogue))
  (setf (claraoke:overrides (claraoke:.text object)) new-value))

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
;;; After initialization
;;;
(defmethod initialize-instance :after ((instance dialogue) &key)
  (let ((value (claraoke:.text instance)))
    (when (stringp value)
      (setf (claraoke:.text instance) (claraoke:text value)))))

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
