(cl:in-package #:asdf-user)

(defsystem "claraoke-subtitle"
  :depends-on ("claraoke-base"
               "claraoke-text"
               "claraoke-color"
               "claraoke-duration")
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "base-implementation")
   (:file "text-implementation")
   (:file "color-implementation")
   (:file "duration-implementation")
   (:file "subtitle-implementation")))

