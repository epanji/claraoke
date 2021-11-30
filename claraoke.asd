(cl:in-package #:asdf-user)

(defsystem "claraoke"
  :version (:read-file-form "version.lisp-expr")
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "System to write KARAOKE typesetting according to ASS format. (Advanced Substation Alpha)"
  :license  "BSD 2-Clause License"
  :in-order-to ((test-op (load-op "claraoke-test")))
  :perform (test-op (o c) (symbol-call :claraoke-test :suite-tests))
  :depends-on ("claraoke-base"
               "claraoke-text"
               "claraoke-color"
               "claraoke-duration"
               "claraoke-subtitle"))

