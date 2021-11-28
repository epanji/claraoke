(cl:in-package #:asdf-user)

(defsystem #:claraoke-test
  :depends-on ("fiveam" "claraoke")
  :components ((:file "tests")))

