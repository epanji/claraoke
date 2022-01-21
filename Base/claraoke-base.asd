(cl:in-package #:asdf-user)

(defsystem "claraoke-base"
  :depends-on ("split-sequence")
  :serial t
  :components
  ((:file "packages")
   (:file "script-protocol")
   (:file "printer-protocol")
   (:file "parser-protocol")
   (:file "text-protocol")
   (:file "color-protocol")
   (:file "duration-protocol")
   (:file "subtitle-protocol")
   (:file "counter-protocol")
   (:file "conditions")
   (:file "utilities")))

