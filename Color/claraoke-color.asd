(cl:in-package #:asdf-user)

(defsystem #:claraoke-color
  :depends-on ("claraoke-base")
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "color-implementation")
   #| ... |#))

