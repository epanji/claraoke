(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subtitle
;;;
(defmethod claraoke:subtitle ((object string) &rest initargs)
  (apply 'make-instance 'subtitle :allow-other-keys t :title object initargs))

(defmethod claraoke:subtitle ((object subtitle) &key)
  object)

(defmethod claraoke:subtitle (object &key)
  (error 'claraoke:failed-to-create-subtitle :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style
;;;
(defmethod claraoke:style ((object string) &rest initargs)
  (apply 'make-instance 'style :allow-other-keys t :name object initargs))

(claraoke-internal:mimic-accessor claraoke:style (claraoke:.style object)
  (error 'claraoke:failed-to-create-style :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Script info
;;;
(defmethod claraoke:script-info ((object string) &rest initargs)
  (apply 'make-instance 'script-info :allow-other-keys t :title object initargs))

(claraoke-internal:mimic-accessor claraoke:script-info (claraoke:.script-info object)
  (error 'claraoke:failed-to-create-script-info :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Event kinds (Dialogue, Comment, picture, etc)
;;;
(macrolet ((define-method-instance (name class)
             `(defmethod ,name ((object string) &rest initargs)
                (apply 'make-instance ',class :allow-other-keys t :text object initargs))))
  (define-method-instance claraoke:dialogue dialogue)
  (define-method-instance claraoke:comment comment)
  (define-method-instance claraoke:picture picture)
  (define-method-instance claraoke:sound sound)
  (define-method-instance claraoke:movie movie)
  (define-method-instance claraoke:command command))

(defmethod claraoke:dialogue ((object claraoke-text:text) &rest initargs)
  (apply 'make-instance 'dialogue :allow-other-keys t :text object initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert style
;;;
(defun same-style-p (style1 style2)
  (check-type style1 claraoke:style)
  (check-type style2 claraoke:style)
  (string-equal (claraoke:name style1) (claraoke:name style2)))

(defmethod claraoke:insert-style ((subtitle subtitle) (style style))
  (pushnew style (claraoke:styles subtitle) :test 'same-style-p))

(defmethod claraoke:insert-style ((subtitle subtitle) style)
  (error 'claraoke:object-must-be-style :object style))

(defmethod claraoke:insert-style (subtitle style)
  (error 'claraoke:object-must-be-subtitle :object subtitle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete style
;;;
(defmethod claraoke:delete-style ((subtitle subtitle) (style style))
  (setf (claraoke:styles subtitle) (remove style (claraoke:styles subtitle))))

(defmethod claraoke:delete-style ((subtitle subtitle) (style null))
  (error 'claraoke:style-not-found :object style))

(defmethod claraoke:delete-style ((subtitle subtitle) (style string))
  (claraoke:delete-style subtitle (claraoke:find-style subtitle style)))

(defmethod claraoke:delete-style ((subtitle subtitle) style)
  (claraoke:delete-style subtitle (write-to-string style)))

(defmethod claraoke:delete-style (subtitle style)
  (error 'claraoke:object-must-be-subtitle :object subtitle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find style
;;;
(defmethod claraoke:find-style ((subtitle subtitle) (style style))
  (find style (claraoke:styles subtitle)))

(defmethod claraoke:find-style ((subtitle subtitle) (style string))
  (find style (claraoke:styles subtitle)
        :key 'claraoke:name
        :test 'string-equal))

(defmethod claraoke:find-style (subtitle style)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert event (Dialogue, Comment, etc)
;;;
(defmethod claraoke:insert-event ((subtitle subtitle) (event event))
  (pushnew event (claraoke:events subtitle)))

(defmethod claraoke:insert-event ((subtitle subtitle) event)
  (error 'claraoke:object-must-be-event :object event))

(defmethod claraoke:insert-event (subtitle event)
  (error 'claraoke:object-must-be-subtitle :object subtitle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete event (Dialogue, Comment, etc)
;;;
(defmethod claraoke:delete-event ((subtitle subtitle) (event event))
  (setf (claraoke:events subtitle) (remove event (claraoke:events subtitle))))

(defmethod claraoke:delete-event ((subtitle subtitle) event)
  (error 'claraoke:object-must-be-event :object event))

(defmethod claraoke:delete-event (subtitle event)
  (error 'claraoke:object-must-be-subtitle :object subtitle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find event (Dialogue, Comment, etc)
;;;
(defmethod claraoke:find-event ((subtitle subtitle) (position integer))
  (let ((events (claraoke:events subtitle)))
    (nth position (reverse events))))

(defmethod claraoke:find-event (subtitle position)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Last event (Dialogue, Comment, etc)
;;;
(defmethod claraoke:last-event ((subtitle subtitle))
  (first (claraoke:events subtitle)))

(defmethod claraoke:last-event (subtitle)
  (error 'claraoke:object-must-be-subtitle :object subtitle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort events (Dialogue, Comment, etc)
;;;
(defun sort-event-predicate (event1 event2)
  (check-type event1 event)
  (check-type event2 event)
  (claraoke:duration-greaterp
   (claraoke:start event1)
   (claraoke:start event2)))

(defmethod claraoke:sort-events ((subtitle subtitle))
  (let ((sorted (claraoke:sort-events (claraoke:events subtitle))))
    (unless (null sorted)
      (setf (claraoke:events subtitle) sorted))
    subtitle))

(defmethod claraoke:sort-events ((subtitle cons))
  (sort subtitle 'sort-event-predicate))

(defmethod claraoke:sort-events ((subtitle null))
  (warn 'claraoke:null-object-warning))

(defmethod claraoke:sort-events (subtitle)
  (error 'claraoke:object-must-be-subtitle :object subtitle))

