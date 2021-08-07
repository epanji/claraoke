;;;; conditions.lisp

(in-package #:claraoke-base)

(define-condition claraoke:null-object-warning (warning)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Provide NULL object always return NIL."))))

(macrolet ((define-object-error (name target format-control reader1 reader2)
             `(define-condition ,name (error)
                ((%object :initarg :object :initform nil :accessor object)
                 (%target :initform ,target :reader target))
                (:report (lambda (condition stream)
                           (format stream ,format-control (,reader1 condition) (,reader2 condition)))))))
  (define-object-error claraoke:style-not-found "STYLE" "~&~A not found with searching input ~A~%" target object)
  (define-object-error claraoke:failed-to-create-subtitle "SUBTITLE" "~&Can not create ~A from ~A.~%" target object)
  (define-object-error claraoke:failed-to-create-color "COLOR" "~&Can not create ~A from ~A.~%" target object)
  (define-object-error claraoke:failed-to-create-duration "DURATION" "~&Can not create ~A from ~A.~%" target object)
  (define-object-error claraoke:failed-to-create-style "STYLE" "~&Can not create ~A from ~A.~%" target object)
  (define-object-error claraoke:failed-to-create-integer "INTEGER" "~&Can not create ~A from ~A.~%" target object)
  (define-object-error claraoke:object-must-be-subtitle "SUBTITLE" "~&Object ~A is not type ~A.~%" object target)
  (define-object-error claraoke:object-must-be-color "COLOR" "~&Object ~A is not type ~A.~%" object target)
  (define-object-error claraoke:object-must-be-duration "DURATION" "~&Object ~A is not type ~A.~%" object target)
  (define-object-error claraoke:object-must-be-style "STYLE" "~&Object ~A is not type ~A.~%" object target)
  (define-object-error claraoke:object-must-be-event "EVENT" "~&Object ~A is not type ~A.~%" object target)
  (define-object-error claraoke:object-must-be-integer "INTEGER" "~&Object ~A is not type ~A.~%" object target)
  (define-object-error claraoke:object-must-be-text "TEXT" "~&Object ~A is not type ~A.~%" object target)
  (define-object-error claraoke:object-must-be-override "OVERRIDE" "~&Object ~A is not type ~A.~%" object target))

