(cl:in-package #:asdf-user)

(defsystem "claraoke-subtitle"
  :depends-on ("claraoke-base" "claraoke-duration" "claraoke-text")
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "subtitle-implementation")))

