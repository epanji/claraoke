(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subtitle
;;;
(defmethod claraoke:subtitle
    ((object string) &rest initargs
     &key
       (title "Untitled")
       (original-script nil)
       (original-translation nil)
       (original-editing nil)
       (original-timing nil)
       (synch-point nil)
       (script-updated-by nil)
       (update-details nil)
       (script-type "v4.00+")
       (collisions "Normal")
       (play-res-x 1280)
       (play-res-y 720)
       (play-depth 0)
       (timer "100.0000")
       (wrap-style 0)
       (scaled-border-and-shadow nil)
       (last-style-storage nil)
       (video-aspect-ratio nil)
       (video-zoom nil)
       (video-position nil)
       (style-name "Default")
       (text "This is first dialogue")
     &allow-other-keys)
  (apply 'make-instance 'subtitle
         :allow-other-keys t
         :title (if (zerop (length object)) title object)
         :original-script original-script
         :original-translation original-translation
         :original-editing original-editing
         :original-timing original-timing
         :synch-point synch-point
         :script-updated-by script-updated-by
         :update-details update-details
         :script-type script-type
         :collisions collisions
         :play-res-x play-res-x
         :play-res-y play-res-y
         :play-depth play-depth
         :timer timer
         :wrap-style wrap-style
         :scaled-border-and-shadow scaled-border-and-shadow
         :last-style-storage last-style-storage
         :video-aspect-ratio video-aspect-ratio
         :video-zoom video-zoom
         :video-position video-position
         :style-name style-name
         :text text
         initargs))

(defmethod claraoke:subtitle ((object subtitle) &key)
  object)

(defmethod claraoke:subtitle ((object null) &key)
  (make-instance 'subtitle))

(defmethod claraoke:subtitle (object &key)
  (error 'claraoke:failed-to-create-subtitle :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sections and Sub Sections
;;;
(macrolet ((define-section-instance (name index class keyword error)
             `(progn
                (defmethod ,name ((object string) &rest initargs)
                  (apply 'make-instance ',class :allow-other-keys t ,keyword object initargs))
                (defmethod ,name ((object subtitle) &key)
                  (aref (claraoke:lines object) ,index))
                (defmethod ,name ((object null) &key)
                  (make-instance ',class))
                (defmethod ,name (object &key)
                  (error ',error :object object)))))
  (define-section-instance claraoke:styles 1 styles :style-name claraoke:failed-to-create-styles)
  (define-section-instance claraoke:events 2 events :text claraoke:failed-to-create-events)
  (define-section-instance claraoke:fonts 3 fonts :fontname claraoke:failed-to-create-fonts)
  (define-section-instance claraoke:graphics 4 graphics :filename claraoke:failed-to-create-graphics))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Script info
;;;
(defmethod claraoke:script-info
    ((object string) &rest initargs
     &key
       (title "Untitled")
       (original-script nil)
       (original-translation nil)
       (original-editing nil)
       (original-timing nil)
       (synch-point nil)
       (script-updated-by nil)
       (update-details nil)
       (script-type "v4.00+")
       (collisions "Normal")
       (play-res-x 1280)
       (play-res-y 720)
       (play-depth 0)
       (timer "100.0000")
       (wrap-style 0)
       (scaled-border-and-shadow nil)
       (last-style-storage nil)
       (video-aspect-ratio nil)
       (video-zoom nil)
       (video-position nil)
     &allow-other-keys)
  (apply 'make-instance 'script-info
         :allow-other-keys t
         :title (if (zerop (length object)) title object)
         :original-script original-script
         :original-translation original-translation
         :original-editing original-editing
         :original-timing original-timing
         :synch-point synch-point
         :script-updated-by script-updated-by
         :update-details update-details
         :script-type script-type
         :collisions collisions
         :play-res-x play-res-x
         :play-res-y play-res-y
         :play-depth play-depth
         :timer timer
         :wrap-style wrap-style
         :scaled-border-and-shadow scaled-border-and-shadow
         :last-style-storage last-style-storage
         :video-aspect-ratio video-aspect-ratio
         :video-zoom video-zoom
         :video-position video-position
         initargs))

(defmethod claraoke:script-info ((object subtitle) &key)
  (aref (claraoke:lines object) 0))

(defmethod claraoke:script-info ((object null) &key)
  (make-instance 'script-info))

(defmethod claraoke:script-info (object &key)
  (error 'claraoke:failed-to-create-script-info :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Note
;;;
(defmethod claraoke:note ((object string) &key)
  (make-instance 'note :descriptor object))

(defmethod claraoke:note ((object note) &key)
  object)

(defmethod claraoke:note (object &key)
  (error 'claraoke:failed-to-create-note :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Info
;;;
(defmethod claraoke:info ((object string) &key value)
  (make-instance 'info :descriptor object :value value))

(defmethod claraoke:info ((object info) &key)
  object)

(defmethod claraoke:info (object &key)
  (error 'claraoke:failed-to-create-info :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Font
;;;
(defmethod claraoke:font ((object string) &key)
  (claraoke:info "fontname" :value object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphic
;;;
(defmethod claraoke:graphic ((object string) &key)
  (claraoke:info "filename" :value object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style
;;;
(defmethod claraoke:style ((object string) &rest initargs)
  (apply 'make-instance 'style :allow-other-keys t :name object initargs))

(defmethod claraoke:style ((object style) &key)
  object)

(claraoke-internal:mimic-accessor claraoke:style (claraoke:.style object)
  (error 'claraoke:failed-to-create-style :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Event
;;;
;;; Dialogue, Comment, Picture, Sound, Movie and
;;; Command
;;;
(macrolet ((define-event-instance (name class)
             `(progn
                (defmethod ,name ((object string) &rest initargs)
                  (apply 'make-instance ',class :allow-other-keys t :text object initargs))
                (defmethod ,name ((object ,class) &key)
                  object))))
  (define-event-instance claraoke:dialogue dialogue)
  (define-event-instance claraoke:comment comment)
  (define-event-instance claraoke:picture picture)
  (define-event-instance claraoke:sound sound)
  (define-event-instance claraoke:movie movie)
  (define-event-instance claraoke:command command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert info
;;;
(defmethod claraoke:insert-info ((object script-info) (info info))
  (pushnew info (claraoke:lines object) :key 'claraoke:descriptor :test 'string-equal))

(defmethod claraoke:insert-info ((object script-info) info)
  (error 'claraoke:object-must-be-info :object info))

(defmethod claraoke:insert-info ((object subtitle) info)
  (claraoke:insert-info (claraoke:script-info object) info))

(defmethod claraoke:insert-info (object info)
  (error 'claraoke:object-must-be-script-info :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete info
;;;
(defmethod claraoke:delete-info ((object script-info) (info info))
  (claraoke-internal:deletef info (claraoke:lines object)))

(defmethod claraoke:delete-info ((object script-info) (info string))
  (claraoke:delete-info object (claraoke:find-info object info)))

(defmethod claraoke:delete-info ((object script-info) info)
  (error 'claraoke:object-must-be-info :object info))

(defmethod claraoke:delete-info ((object subtitle) info)
  (claraoke:delete-info (claraoke:script-info object) info))

(defmethod claraoke:delete-info (object info)
  (error 'claraoke:object-must-be-script-info :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find info
;;;
(defmethod claraoke:find-info ((object script-info) (info info))
  (find info (claraoke:lines object)))

(defmethod claraoke:find-info ((object script-info) (info string))
  (find info (claraoke:lines object) :key 'claraoke:descriptor :test 'string-equal))

(defmethod claraoke:find-info ((object script-info) info)
  nil)

(defmethod claraoke:find-info ((object subtitle) info)
  (claraoke:find-info (claraoke:script-info object) info))

(defmethod claraoke:find-info (object info)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert note
;;;
(defmethod claraoke:insert-note ((object claraoke:script) (note note))
  (pushnew note (claraoke:lines object) :key 'claraoke:descriptor :test 'string=))

(defmethod claraoke:insert-note ((object claraoke:script) note)
  (error 'claraoke:object-must-be-note :object note))

(defmethod claraoke:insert-note ((object subtitle) note)
  (claraoke:insert-note (claraoke:script-info object) note))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete note
;;;
(defmethod claraoke:delete-note ((object subtitle) note)
  (claraoke:delete-line (claraoke:script-info object) note))

(defmethod claraoke:delete-note (object note)
  (claraoke:delete-line object note))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find note
;;;
(defmethod claraoke:find-note ((object subtitle) note)
  (claraoke:find-line (claraoke:script-info object) note))

(defmethod claraoke:find-note (object note)
  (claraoke:find-line object note))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert style
;;;
(defun same-style-p (style1 style2)
  (check-type style1 style)
  (check-type style2 style)
  (string-equal (claraoke:name style1) (claraoke:name style2)))

(defmethod claraoke:insert-style ((object styles) (style style))
  (pushnew style (claraoke:lines object) :test 'same-style-p))

(defmethod claraoke:insert-style ((object styles) style)
  (error 'claraoke:object-must-be-style :object style))

(defmethod claraoke:insert-style ((object subtitle) style)
  (claraoke:insert-style (claraoke:styles object) style))

(defmethod claraoke:insert-style (object style)
  (error 'claraoke:object-must-be-styles :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete style
;;;
(defmethod claraoke:delete-style ((object styles) (style style))
  (claraoke-internal:deletef style (claraoke:lines object)))

(defmethod claraoke:delete-style ((object styles) (style string))
  (claraoke:delete-style object (claraoke:find-style object style)))

(defmethod claraoke:delete-style ((object styles) style)
  nil)

(defmethod claraoke:delete-style ((object subtitle) style)
  (claraoke:delete-style (claraoke:styles object) style))

(defmethod claraoke:delete-style (object style)
  (error 'claraoke:object-must-be-styles :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find style
;;;
(defmethod claraoke:find-style ((object styles) (style style))
  (find style (claraoke:lines object)))

(defmethod claraoke:find-style ((object styles) (style string))
  (find style (claraoke:lines object) :key 'claraoke:name :test 'string-equal))

(defmethod claraoke:find-style ((object styles) style)
  nil)

(defmethod claraoke:find-style ((object subtitle) style)
  (claraoke:find-style (claraoke:styles object) style))

(defmethod claraoke:find-style (object style)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert event (Dialogue, Comment, etc)
;;;
(defmethod claraoke:insert-event ((object events) (event event) &key interval-delay interval-event)
  (when (integerp (claraoke:interval-counter object))
    (check-type interval-delay (or null integer))
    (check-type interval-event (or null integer))
    (let* ((interval (claraoke:interval object))
           (delta-delay (* interval (or interval-delay 1)))
           (delta-event (* interval (or interval-event (claraoke:interval-frequency object)))))
      (setf (claraoke:start event) (incf (claraoke:interval-counter object) delta-delay))
      (setf (claraoke:end event) (incf (claraoke:interval-counter object) delta-event))))
  (pushnew event (claraoke:lines object)))

(defmethod claraoke:insert-event ((object events) event &key)
  (error 'claraoke:object-must-be-event :object event))

(defmethod claraoke:insert-event ((object subtitle) event &key interval-delay interval-event)
  (claraoke:insert-event (claraoke:events object) event :interval-delay interval-delay :interval-event interval-event))

(defmethod claraoke:insert-event (object event &key)
  (error 'claraoke:object-must-be-events :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete event (Dialogue, Comment, etc)
;;;
(defmethod claraoke:delete-event ((object events) (event event))
  (claraoke-internal:deletef event (claraoke:lines object)))

(defmethod claraoke:delete-event ((object events) event)
  (error 'claraoke:object-must-be-event :object event))

(defmethod claraoke:delete-event ((object subtitle) event)
  (claraoke:delete-event (claraoke:events object) event))

(defmethod claraoke:delete-event (object event)
  (error 'claraoke:object-must-be-events :object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find event (Dialogue, Comment, etc)
;;;
(defmethod claraoke:find-event ((object events) (index integer))
  (let ((lines (claraoke:lines object)))
    (nth index (reverse lines))))

(defmethod claraoke:find-event ((object events) index)
  nil)

(defmethod claraoke:find-event ((object subtitle) index)
  (claraoke:find-event (claraoke:events object) index))

(defmethod claraoke:find-event (subtitle index)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert font
;;;
(defmethod claraoke:insert-font ((object fonts) (font info))
  (claraoke:insert-line object font))

(defmethod claraoke:insert-font ((object subtitle) font)
  (claraoke:insert-font (claraoke:fonts object) font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete font
;;;
(defmethod claraoke:delete-font ((object fonts) (font info))
  (claraoke:delete-line object font))

(defmethod claraoke:delete-font ((object subtitle) font)
  (claraoke:delete-font (claraoke:fonts object) font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find font
;;;
(defmethod claraoke:find-font ((object fonts) (font info))
  (claraoke:find-line object font))

(defmethod claraoke:find-font ((object subtitle) font)
  (claraoke:find-font (claraoke:fonts object) font))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insert graphic
;;;
(defmethod claraoke:insert-graphic ((object graphics) (graphic info))
  (claraoke:insert-line object graphic))

(defmethod claraoke:insert-graphic ((object subtitle) graphic)
  (claraoke:insert-graphic (claraoke:graphics object) graphic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Delete graphic
;;;
(defmethod claraoke:delete-graphic ((object graphics) (graphic info))
  (claraoke:delete-line object graphic))

(defmethod claraoke:delete-graphic ((object subtitle) graphic)
  (claraoke:delete-graphic (claraoke:graphics object) graphic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find graphic
;;;
(defmethod claraoke:find-graphic ((object graphics) (graphic info))
  (claraoke:find-line object graphic))

(defmethod claraoke:find-graphic ((object subtitle) graphic)
  (claraoke:find-graphic (claraoke:graphics object) graphic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Last event (Dialogue, Comment, etc)
;;;
(defmethod claraoke:last-event ((object events))
  (first (claraoke:lines object)))

(defmethod claraoke:last-event (object)
  (error 'claraoke:object-must-be-events :object object))

(defmethod claraoke:last-event ((object subtitle))
  (claraoke:last-event (claraoke:events object)))

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

(defmethod claraoke:sort-events ((object events))
  (sort (claraoke:lines object) 'sort-event-predicate))

(defmethod claraoke:sort-events ((object subtitle))
  (claraoke:sort-events (claraoke:events object)))

(defmethod claraoke:sort-events ((object null))
  (warn 'claraoke:null-object-warning))

(defmethod claraoke:sort-events (object)
  (error 'claraoke:object-must-be-events :object object))

