(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert override
;;;
(defmethod claraoke:insert-override ((object claraoke:dialogue) override)
  (claraoke:insert-override (claraoke:text object) override)
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete override
;;;
(defmethod claraoke:delete-override ((object claraoke:dialogue) override)
  (claraoke:delete-override (claraoke:text object) override)
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find override
;;;
(defmethod claraoke:find-override ((object claraoke:dialogue) override)
  (claraoke:find-override (claraoke:text object) override))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort override
;;;
(defmethod claraoke:sort-overrides ((object claraoke:dialogue))
  (claraoke:sort-overrides (claraoke:text object))
  object)
