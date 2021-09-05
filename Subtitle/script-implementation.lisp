(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert line
;;;
(defmethod claraoke:insert-line ((script claraoke:script) (line claraoke:line))
  (pushnew line (claraoke:lines script)))

(defmethod claraoke:insert-line ((script claraoke:script) line)
  (error 'claraoke:object-must-be-line :object line))

(defmethod claraoke:insert-line (script line)
  (error 'claraoke:object-must-be-script :object script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete line
;;;
(defmethod claraoke:delete-line ((script claraoke:script) (line claraoke:line))
  (claraoke-internal:deletef line (claraoke:lines script)))

(defmethod claraoke:delete-line ((script claraoke:script) line)
  (error 'claraoke:object-must-be-line :object line))

(defmethod claraoke:delete-line (script line)
  (error 'claraoke:object-must-be-script :object script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find line
;;;
(defmethod claraoke:find-line ((script claraoke:script) (line string))
  (find line (claraoke:lines script) :key 'claraoke:descriptor :test 'string=))

(defmethod claraoke:find-line ((script claraoke:script) (line claraoke:line))
  (find line (claraoke:lines script)))

(defmethod claraoke:find-line ((script claraoke:script) line)
  (error 'claraoke:object-must-be-line :object line))

(defmethod claraoke:find-line (script line)
  (error 'claraoke:object-must-be-script :object script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Last line
;;;
(defmethod claraoke:last-line ((script claraoke:script))
  (first (claraoke:lines script)))

(defmethod claraoke:last-line (script)
  (error 'claraoke:object-must-be-script :object script))

