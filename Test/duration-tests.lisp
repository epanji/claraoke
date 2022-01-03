(cl:in-package #:claraoke-test)

(in-suite duration-suite)

(test duration-from-integer
  (is (durationp (duration 1234567))))

(test duration-from-string
  (is (durationp (duration "Wrong format lead to zero duration")))
  (is (durationp (duration "1")))
  (is (durationp (duration "1:2")))
  (is (durationp (duration "1:2:3")))
  (is (durationp (duration "1:2:3.4")))
  (is (durationp (duration "1:2:3.45")))
  (is (durationp (duration "123456"))))

(test integer-from-various-data
  (let* ((int 1234567)
         (str "3:25:45.67")
         (obj (duration str)))
    (is (integerp (durationinteger int)))
    (is (integerp (durationinteger str)))
    (is (integerp (durationinteger obj)))))

(test comparing-two-various-data
  (let ((obj (duration "1:23")))
    (is (= (durationinteger 1234) (durationinteger "1234")))
    (is (= (durationinteger 1234567) (durationinteger "3:25:45.67")))
    (is (= 1111 (duration-difference "123" 1234)))
    (is (= 1111 (duration-difference 1234 "123")))
    (is (= 6080 (duration-difference 123 "1:2.3")))
    (is (= 6080 (duration-difference "1:2.3" 123)))
    (is (duration-greaterp "1:23" 123))
    (is (duration-greaterp obj 123))
    (is (duration-lessp "1:23" 12345))
    (is (duration-lessp obj 12345))))

(test mutating-duration
  (let ((obj (duration "1234")))
    (is (durationp (increase-duration obj "1:2:3.4")))
    (is (> (durationinteger obj) 1234))
    (is (durationp (decrease-duration obj "1:2:3.4")))
    (is (= (durationinteger obj) 1234))
    (is (durationp (decrease-duration obj "1:2:3.4")))
    (is (zerop (durationinteger obj)))))

(test string-duration-from-various-data
  (let ((obj (duration 1234567)))
    (is (stringp (durationstring obj)))
    (is (stringp (durationstring 1234567)))
    (is (stringp (durationstring "Wrong format lead to zero duration")))
    (is (stringp (durationstring "1")))
    (is (stringp (durationstring "1:2")))
    (is (stringp (durationstring "1:2:3")))
    (is (stringp (durationstring "1:2:3.4")))
    (is (stringp (durationstring "1:2:3.45")))
    (is (stringp (durationstring "123456")))))

(test string-duration-predicate
  (is-true (durationstringp "0:01:02.03"))
  (is-true (durationstringp "123:01:02.03"))
  (is-false (durationstringp "0:01:02.123"))
  (is-false (durationstringp "0:1:2.3"))
  (is-false (durationstringp "123"))
  (is-true (durationstringp (durationstring "0:1:2.3")))
  (is-true (durationstringp (durationstring "123"))))

(test expecting-duration-error
  (signals error (duration 1.2f0))
  (signals error (duration 1.2d0)))

