(cl:in-package #:asdf-user)

(defsystem "claraoke-text"
  :depends-on ("claraoke-base")
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "text-implementation")
   #|...|#))

