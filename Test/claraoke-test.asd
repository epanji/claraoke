(cl:in-package #:asdf-user)

(defsystem #:claraoke-test
  :depends-on ("fiveam" "claraoke")
  :serial t
  :components
  ((:file "package")
   (:file "suites")
   (:file "duration-tests")
   (:file "color-tests")
   (:file "text-tests")
   (:file "subtitle-tests")))

