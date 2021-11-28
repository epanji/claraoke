(cl:in-package #:cl-user)

(defpackage #:claraoke-test
  (:use #:common-lisp #:fiveam #:claraoke)
  (:export #:suite-tests))

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

(test expecting-duration-error
  (signals error (duration 1.2f0))
  (signals error (duration 1.2d0)))

(in-suite color-suite)

(test color-from-integer
  (is (colorp (rgb 12 34 56)))
  (is (colorp (rgb 12 34 56 77))))

(test color-from-string-name
  (is (colorp (color "Wrong name lead to zero color")))
  (is (colorp (color "blue-violet")))
  (is (colorp (color "blue violet")))
  (is (colorp (color "Blue Violet")))
  (is (colorp (color "blueviolet")))
  (is (colorp (color "BlueViolet"))))

(test color-from-string-html
  (is (colorp (color "#8A2BE2")))
  (is (colorp (color "#8A2BE24D"))))

(test color-from-string-spec
  (is (colorp (color "&HE22B8A&")))
  (is (colorp (color "&H4DE22B8A"))))

(test color-from-random
  (is (colorp (random-color)))
  (is (colorp (random-color 3/10))))

(test alpha-from-various-data
  (let ((obj (color "#8A2BE24D")))
    (is (integerp (alpha obj)))
    (is (integerp (alpha 77)))
    (is (integerp (alpha 3/10)))
    (is (integerp (alpha "4D")))))

(test string-alpha-from-various-data
  (is (stringp (alphastring 77)))
  (is (stringp (alphastring 3/4)))
  (is (stringp (alphastring "4D")))
  (is (stringp (alphastring "#4D")))
  (is (stringp (alphastring "&H4D")))
  (is (stringp (alphastring "&H4D&"))))

(test string-color-from-various-data
  (let ((obj (color "#8A2BE24D")))
    (is (stringp (colorstring obj)))
    (is (stringp (colorstring "Wrong name lead to zero color")))
    (is (stringp (colorstring "blue-violet")))
    (is (stringp (colorstring "blue violet")))
    (is (stringp (colorstring "Blue Violet")))
    (is (stringp (colorstring "blueviolet")))
    (is (stringp (colorstring "BlueViolet")))
    (is (stringp (colorstring "#8A2BE2")))
    (is (stringp (colorstring "#8A2BE24D")))
    (is (stringp (colorstring "&HE22B8A&")))
    (is (stringp (colorstring "&H4DE22B8A")))))

(test expecting-color-error
  (signals error (colorstring 1234567))
  (signals error (colorstring #x8A2BE2)))

(in-suite text-suite)

(test creating-text-with-various-keyargs
  (let ((txt1 (ps-string (text "Hello world!")))
        (txt2 (ps-string (text "Hello world!" :generate-overrides-p t)))
        (txt3 (ps-string (text "Hello world!" :generate-overrides-p t :spell-duration 50)))
        (txt4 (ps-string (text "Hello world!" :generate-overrides-p t :spell-duration 50 :change-karaoke-type :fill)))
        (txt5 (ps-string (text "Hello world!" :generate-overrides-p t :spell-duration 50 :change-karaoke-type :outline)))
        (txt6 (ps-string (text "{\\None\\K25}Hel{\\K50}lo {\\K100}world!")))
        (txt7 (ps-string (text "{\\None\\K25}Hel{\\K50}lo {\\K100}world!" :remove-unknown-modifier-p t)))
        (txt8 (ps-string (text "{\\None\\K25}Hel{\\K50}lo {\\K100}world!" :remove-unknown-modifier-p t :keep-original-modifier-p t))))
    (is (string= "Hello world!" txt1))
    (is (string= "{\\k15}Hel{\\k15}lo {\\k15}world!" txt2))
    (is (string= "{\\k50}Hel{\\k50}lo {\\k50}world!" txt3))
    (is (string= "{\\kf50}Hel{\\kf50}lo {\\kf50}world!" txt4))
    (is (string= "{\\ko50}Hel{\\ko50}lo {\\ko50}world!" txt5))
    (is (string= "{\\None\\kf25}Hel{\\kf50}lo {\\kf100}world!" txt6))
    (is (string= "{\\kf25}Hel{\\kf50}lo {\\kf100}world!" txt7))
    (is (string= "{\\K25}Hel{\\K50}lo {\\K100}world!" txt8))))

(test mutating-text-with-new-string
  (let ((obj (text "Hello world!" :generate-overrides-p t)))
    (is (stringp (setf (text obj) "Wonderfull")))
    (is (string= "{\\k15}Won{\\k15}der{\\k15}full" (ps-string obj)))))

(test working-with-karaoke
  (let ((txt1 (text "Hello world!"))
        (txt2 (text "Hello world!" :generate-overrides-p t)))
    ;; Functions insert-karaoke* free to add any karaoke type without
    ;; care about existing karaoke or override existences
    (insert-karaoke txt1 0 12)
    (insert-karaoke-fill txt1 "lo w" 13)
    (insert-karaoke-outline txt1 "world" 14)
    (insert-karaoke-outline txt1 "world")
    (is (string= "{\\k12}Hel{\\kf13}lo {\\ko15}world!" (ps-string txt1)))
    ;; Functions update-karaoke, decrease-karaoke and increase-karaoke
    ;; only change value without changing karaoke type and restricted
    ;; by karaoke existences
    (update-karaoke (find-override txt2 0) 12)
    (decrease-karaoke (find-override txt2 "lo w") 2)
    (decrease-karaoke (find-override txt2 "world") 3)
    (increase-karaoke (find-override txt2 "world") 3)
    (is (string= "{\\k12}Hel{\\k13}lo {\\k15}world!" (ps-string txt2)))))

(test working-with-overrides-and-modifiers
  (let* ((txt1 (text "Hello world!"
                     :overrides
                     (list
                      ;; Batch with list modifiers
                      (override 'batch 0
                                :modifiers
                                (list (modifier 'karaoke :arg1 12)
                                      (override 'fontsize nil :arg1 18)))
                      ;; Batch with one modifier plus list modifiers
                      (override 'karaoke-fill 3
                                :arg1 13
                                :modifiers
                                (list (modifier 'fontspace :arg1 1.5)))
                      ;; Batch with one modifier
                      (override 'karaoke-outline 6 :arg1 15))))
         (ovr2 (find-override txt1 3))
         (ovr4 (progn
                 (insert-override txt1 (override 'fontsize 8 :arg1 10))
                 (find-override txt1 8)))
         (mod4 (find-modifier ovr4 'fontsize)))
    (is-true (consp (sort-overrides txt1)))
    (is (= 10 (arg1 mod4)))
    (is (= 11 (arg1 (increase-modifier mod4))))
    (is (= 10 (arg1 (decrease-modifier mod4))))
    (is (string= "{\\k12\\fs18}Hel{\\kf13\\fsp1.5}lo {\\ko15}wo{\\fs10}rld!" (ps-string txt1)))
    (is (= 8 (index (delete-modifier ovr4 mod4))))
    (is (= 4 (length (overrides txt1))))
    ;; Empty batch will not be printed
    (is (string/= "{\\k12\\fs18}Hel{\\kf13\\fsp1.5}lo {\\ko15}wo{}rld!" (ps-string txt1)))
    (is (= 8 (index (insert-modifier ovr4 (modifier 'bold :arg1 0)))))
    (is (string= "{\\k12\\fs18}Hel{\\kf13\\fsp1.5}lo {\\ko15}wo{\\b0}rld!" (ps-string txt1)))
    (is (= 3 (length (overrides (delete-override txt1 ovr4)))))
    (is (= 4 (increase-override ovr2)))
    (is (= 3 (decrease-override ovr2)))
    (is (string= "{\\k12\\fs18}Hel{\\kf13\\fsp1.5}lo {\\ko15}world!" (ps-string txt1)))
    (is (zerop (index (find-override txt1 0))))
    (is (= 3 (index (find-override txt1 "lo w"))))
    (is (= 6 (index (find-override txt1 "world"))))))

(test checking-default-modifiers
  (is (string= "\\a2" (po-string (modifier 'alignment))))
  (is (string= "\\an8" (po-string (modifier 'alignment-numpad))))
  (is (string= "\\alpha&H00&" (po-string (modifier 'alpha))))
  (is (string= "\\1a&H00&" (po-string (modifier 'alpha1))))
  (is (string= "\\2a&H00&" (po-string (modifier 'alpha2))))
  (is (string= "\\3a&H00&" (po-string (modifier 'alpha3))))
  (is (string= "\\4a&H00&" (po-string (modifier 'alpha4))))
  (is (string= "\\be1" (po-string (modifier 'blur-edges))))
  (is (string= "\\blur1" (po-string (modifier 'blur))))
  (is (string= "\\b1" (po-string (modifier 'bold))))
  (is (string= "\\bord1" (po-string (modifier 'border))))
  (is (string= "\\xbord1" (po-string (modifier 'border-x))))
  (is (string= "\\ybord1" (po-string (modifier 'border-y))))
  (is (string= "\\clip(m 0 0 l 100 0 100 100 0 100)" (po-string (modifier 'clip-drawing))))
  (is (string= "\\clip(1,m 0 0 l 100 0 100 100 0 100)" (po-string (modifier 'clip-drawing-scaled))))
  (is (string= "\\clip(1,1,100,100)" (po-string (modifier 'clip-rectangle))))
  (is (string= "\\c&HFF0011&" (po-string (modifier 'color))))
  (is (string= "\\1c&HFF0011&" (po-string (modifier 'color1))))
  (is (string= "\\2c&HFF0011&" (po-string (modifier 'color2))))
  (is (string= "\\3c&HFF0011&" (po-string (modifier 'color3))))
  (is (string= "\\4c&HFF0011&" (po-string (modifier 'color4))))
  (is (string= "\\pbo1" (po-string (modifier 'drawing-baseline-offset))))
  (is (string= "\\p1" (po-string (modifier 'drawing-mode))))
  (is (string= "\\fad(1200,250)" (po-string (modifier 'fad))))
  (is (string= "\\fade(255,32,224,0,500,2000,2200)" (po-string (modifier 'fade))))
  (is (string= "\\fe1" (po-string (modifier 'fontencoding))))
  (is (string= "\\fnArial" (po-string (modifier 'fontname))))
  (is (string= "\\fr30" (po-string (modifier 'fontrotate))))
  (is (string= "\\frx30" (po-string (modifier 'fontrotate-x))))
  (is (string= "\\fry30" (po-string (modifier 'fontrotate-y))))
  (is (string= "\\frz30" (po-string (modifier 'fontrotate-z))))
  (is (string= "\\fscx100.1" (po-string (modifier 'fontscale-x))))
  (is (string= "\\fscy100.1" (po-string (modifier 'fontscale-y))))
  (is (string= "\\fax1" (po-string (modifier 'fontshear-x))))
  (is (string= "\\fay1" (po-string (modifier 'fontshear-y))))
  (is (string= "\\fs36" (po-string (modifier 'fontsize))))
  (is (string= "\\fsp0" (po-string (modifier 'fontspace))))
  (is (string= "\\iclip(m 0 0 l 100 0 100 100 0 100)" (po-string (modifier 'iclip-drawing))))
  (is (string= "\\iclip(1,m 0 0 l 100 0 100 100 0 100)" (po-string (modifier 'iclip-drawing-scaled))))
  (is (string= "\\iclip(1,1,100,100)" (po-string (modifier 'iclip-rectangle))))
  (is (string= "\\i1" (po-string (modifier 'italic))))
  (is (string= "\\K15" (po-string (modifier 'karaoke-capital))))
  (is (string= "\\kf15" (po-string (modifier 'karaoke-fill))))
  (is (string= "\\k15" (po-string (modifier 'karaoke))))
  (is (string= "\\ko15" (po-string (modifier 'karaoke-outline))))
  (is (string= "\\move(100,150,300,350)" (po-string (modifier 'move))))
  (is (string= "\\n" (po-string (modifier 'newline))))
  (is (string= "\\org(320,240)" (po-string (modifier 'origin))))
  (is (string= "\\pos(320,240)" (po-string (modifier 'pos))))
  (is (string= "\\r" (po-string (modifier 'reset))))
  (is (string= "\\shad1" (po-string (modifier '.shadow))))
  (is (string= "\\xshad1" (po-string (modifier 'shadow-x))))
  (is (string= "\\yshad1" (po-string (modifier 'shadow-y))))
  (is (string= "\\s1" (po-string (modifier 'strikeout))))
  (is (string= "\\t(\\frz30)" (po-string (modifier 'transformation1))))
  (is (string= "\\t(0.5,\\frz30)" (po-string (modifier 'transformation2))))
  (is (string= "\\t(300,400,\\frz30)" (po-string (modifier 'transformation3))))
  (is (string= "\\t(300,400,0.5,\\frz30)" (po-string (modifier 'transformation4))))
  (is (string= "\\u1" (po-string (modifier 'underline))))
  (is (string= "\\q1" (po-string (modifier 'wrapping-style)))))

(in-suite subtitle-suite)

(defparameter *subtitle-string* (format nil "~
[Script Info]
; Script generated by ~A ( https://github.com/epanji/claraoke )
Title: Test
ScriptType: v4.00+
Collisions: Normal
PlayResX: 1280
PlayResY: 720
PlayDepth: 0
Timer: 100.0000
WrapStyle: 0

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
Style: Default,Arial,32,&H0000A5FF,&H00FFFFFF,&H00000000,&H00000000,-1,0,0,0,100,100,0,0,1,1,1,2,25,25,72,1

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text
Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,This is first dialogue~2%"
                                  (claraoke-internal:version)))

(test working-with-subtitle
  (let ((sub (subtitle "Test"))
        (stream (make-string-output-stream)))
    (is (string-equal *subtitle-string* (ps-string sub)))
    (is (eql 'claraoke-subtitle:subtitle (type-of sub)))
    (is (eql 'claraoke-subtitle:subtitle (type-of (print-script sub stream))))
    (is (eql 'claraoke-subtitle:subtitle (type-of (parse-script *subtitle-string*)))))
  (let ((sub (subtitle "" :title "Conf Script Info" :collisions "Reverse")))
    (is (string= "Conf Script Info" (value (find-info sub "Title"))))
    (is (string= "Reverse" (value (find-info sub "Collisions")))))
  (let ((sub (subtitle "Conf Default Style" :fontname "DejaVu San" :style-margin-v 25)))
    (is (string= "DejaVu San" (fontname (find-style sub "Default"))))
    (is (= 25 (margin-v (find-style sub "Default")))))
  (let ((sub1 (subtitle "Conf First Dialogue" :text "Hello world!"))
        (sub2 (subtitle "Conf First Dialogue" :text nil))
        (sub3 (subtitle nil)))
    (is (string= "Hello world!" (ps-string (text (last-event sub1)))))
    (is (null (lines (events sub2))))
    (is (null (lines (events sub3))))
    (is (null (lines (styles sub3))))))

(test working-with-script-info-section
  (let* ((sub (subtitle "Script Info" :play-res-x 640 :play-res-y 320 ))
         (cnt (length (lines (script-info sub)))))
    (is (= 640 (value (find-info sub "PlayResX"))))
    (is (= 320 (value (find-info sub "PlayResY"))))
    (is (= 200 (setf (value (find-info sub "PlayResX")) 200)))
    (is (= 100 (setf (value (find-info sub "PlayResY")) 100)))
    (insert-info sub (info "Unknown" :value "Ignored"))
    (is (= (1+ cnt) (length (lines (script-info sub)))))
    (is (string= "Ignored" (value (find-info sub "Unknown"))))
    (delete-info sub (find-info sub "Unknown"))
    (is (= cnt (length (lines (script-info sub)))))))

(test working-with-styles-section
  (let ((sub (subtitle "Styles" :alignment 2)))
    (is (= 1 (length (lines (styles sub)))))
    (insert-style sub (style "Top Centered" :alignment 8))
    (is (= 2 (length (lines (styles sub)))))
    (delete-style sub (find-style sub "Top Centered"))
    (is (= 1 (length (lines (styles sub)))))))

(test working-with-events-section
  (let ((sub (subtitle "Dialogue" :text nil)))
    (is (= 10 (setf (interval sub) 10)))
    (is (= 8 (setf (interval-frequency sub) 8)))
    (is (= 80 (setf (interval-counter sub) 80)))
    (insert-event sub (dialogue "Hello world!"))
    (is (= 90 (durationinteger (start (last-event sub)))))
    (is (= 170 (durationinteger (end (last-event sub)))))
    (insert-event sub (dialogue "Hello world!") :interval-delay 0 :interval-event 9)
    (is (= 170 (durationinteger (start (last-event sub)))))
    (is (= 260 (durationinteger (end (last-event sub)))))))

(test creating-dialogue-with-various-keyargs
  (let ((dlg1 (ps-string (dialogue "Hello world!")))
        (dlg2 (ps-string (dialogue "Hello world!" :generate-overrides-p t)))
        (dlg3 (ps-string (dialogue "Hello world!" :generate-overrides-p t :spell-duration 50)))
        (dlg4 (ps-string (dialogue "Hello world!" :generate-overrides-p t :spell-duration 50 :change-karaoke-type :fill)))
        (dlg5 (ps-string (dialogue "Hello world!" :generate-overrides-p t :spell-duration 50 :change-karaoke-type :outline)))
        (dlg6 (ps-string (dialogue "{\\None\\K25}Hel{\\K50}lo {\\K100}world!")))
        (dlg7 (ps-string (dialogue "{\\None\\K25}Hel{\\K50}lo {\\K100}world!" :remove-unknown-modifier-p t)))
        (dlg8 (ps-string (dialogue "{\\None\\K25}Hel{\\K50}lo {\\K100}world!" :remove-unknown-modifier-p t :keep-original-modifier-p t))))
    (is (string= (format nil "Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,Hello world!~%") dlg1))
    (is (string= (format nil "Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,{\\k15}Hel{\\k15}lo {\\k15}world!~%") dlg2))
    (is (string= (format nil "Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,{\\k50}Hel{\\k50}lo {\\k50}world!~%") dlg3))
    (is (string= (format nil "Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,{\\kf50}Hel{\\kf50}lo {\\kf50}world!~%") dlg4))
    (is (string= (format nil "Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,{\\ko50}Hel{\\ko50}lo {\\ko50}world!~%") dlg5))
    (is (string= (format nil "Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,{\\None\\kf25}Hel{\\kf50}lo {\\kf100}world!~%") dlg6))
    (is (string= (format nil "Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,{\\kf25}Hel{\\kf50}lo {\\kf100}world!~%") dlg7))
    (is (string= (format nil "Dialogue: 0,0:00:00.00,0:00:03.00,Default,,0,0,0,,{\\K25}Hel{\\K50}lo {\\K100}world!~%") dlg8)))
  (let ((dlg1 (dialogue "Hello world!" :start 100 :end 400))
        (dlg2 (dialogue "Hello world!" :start 100 :duration 300))
        (dlg3 (dialogue "Hello world!" :end 300))
        (dlg4 (dialogue "Hello world!" :duration 300))
        (dlg5 (dialogue "Hello world!")))
    (= (durationinteger (end dlg1)) (durationinteger (end dlg2)))
    (= (durationinteger (duration-length dlg1)) (durationinteger (duration-length dlg2)))
    (= (durationinteger (duration-length dlg1)) (durationinteger (duration-length dlg3)))
    (= (durationinteger (duration-length dlg1)) (durationinteger (duration-length dlg4)))
    (= (durationinteger (duration-length dlg2)) (durationinteger (duration-length dlg3)))
    (= (durationinteger (duration-length dlg2)) (durationinteger (duration-length dlg4)))
    (= (durationinteger (duration-length dlg3)) (durationinteger (duration-length dlg4)))
    (setf (start dlg5) 900)
    ;; Ensure time end always greater than time start after updating
    (is (duration-greaterp (end dlg5) (start dlg5)))
    (setf (duration-length dlg5) 300)
    (is (string= (format nil "Dialogue: 0,0:00:09.00,0:00:12.00,Default,,0,0,0,,Hello world!~%") (ps-string dlg5)))))

(test working-with-fonts-section
  (let ((sub (subtitle "Fonts" :font-filename "myfont_B0.ttf")))
    ;; No embedding here, only add filename for editing later
    (is (string= (format nil "[Fonts]~%fontname: myfont_B0.ttf~2%") (ps-string (fonts sub))))))

(test working-with-graphics-section
  (let ((sub (subtitle "Graphics" :graphic-filename "mypic.jpg")))
    (is (string= (format nil "[Graphics]~%filename: mypic.jpg~2%") (ps-string (graphics sub))))))

