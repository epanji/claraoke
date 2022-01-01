(cl:in-package #:claraoke-test)

(defun suite-tests ()
  (run! 'duration-suite)
  (run! 'color-suite)
  (run! 'text-suite)
  (run! 'subtitle-suite))

(defmacro ps-string (form)
  `(with-output-to-string (stream)
     (print-script ,form stream)))

(defmacro po-string (form)
  `(with-output-to-string (stream)
     (claraoke-text:print-override ,form stream)))

(def-suite duration-suite :description "Claraoke duration test suite.")
(def-suite color-suite :description "Claraoke color test suite.")
(def-suite text-suite :description "Claraoke text test suite.")
(def-suite subtitle-suite :description "Claraoke subtitle test suite.")

