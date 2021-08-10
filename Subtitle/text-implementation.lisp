(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors
;;;
(defmethod claraoke:overrides ((object dialogue))
  (claraoke:overrides (claraoke:.text object)))

(defmethod (setf claraoke:overrides) (new-value (object dialogue))
  (setf (claraoke:overrides (claraoke:.text object)) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; After initialization
;;;
(defmethod initialize-instance :after ((instance dialogue) &key &allow-other-keys)
  (let ((string (claraoke:.text instance)))
    (setf (claraoke:.text instance) (claraoke:text string))))

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
