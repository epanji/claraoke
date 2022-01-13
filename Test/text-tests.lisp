(cl:in-package #:claraoke-test)

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

(test merging-empty-syllable
  (let ((str1 (ps-string (text "{\\fr30}Hello {\\r}{\\frz-30}world!")))
        (str2 (ps-string (text "{\\fr30}Hello {\\r}{\\frz-30}world!" :keep-original-modifier-p t)))
        (str3 (ps-string (text "{\\fr30}Hello {\\r}{\\frz-30}world!" :keep-original-modifier-p t :remove-unknown-modifier-p t))))
    (is (string= "{\\frz30}Hello {\\r\\frz-30}world!" str1))
    (is (string= "{\\fr30}Hello {\\r\\frz-30}world!" str2))
    (is (string= "{\\fr30}Hello {\\r\\frz-30}world!" str3))))

(test keep-last-duplicate-unique-modifier
  ;; Assuming first unique modifier is a mistake from overridden text
  (let ((str1 (ps-string (text "{\\k15}Hel{\\k15}lo {\\k15\\1c&H0000FF&}{\\k30\\1c&H00FFFF&}world!")))
        (str2 (ps-string (text "{\\k15}Hel{\\k15}lo {\\k15\\1c&H0000FF&\\k30\\1c&H00FFFF&}world!")))
        ;; Although it is possible to write "{\\k100\\k30}syllable" to
        ;; delay 1 seconds before animate "syllable", it is better to
        ;; adjust the dialogue time or separate override with
        ;; invisible syllable character (#\INVISIBLE_SEPARATOR)
        (str3 "{\\k15}Hel{\\k15}lo {\\k100}⁣{\\k30\\1c&H00FFFF&}world!")
        (str4 "{\\k15}Hel{\\k15}lo {\\t(0,200,\\fry-80)\\t(200,300,\\1c&H0000FF)\\t(300,400,\\fry0\\1c&H00FFFF&)}world!"))
    (is (string= "{\\k15}Hel{\\k15}lo {\\k30\\1c&H00FFFF&}world!" str1))
    (is (string= str1 str2))
    (is (string= str3 (ps-string (text str3))))
    (is (string= str4 (ps-string (text str4))))))

(test working-with-karaoke
  (let ((txt1 (text "Hello world!"))
        (txt2 (text "Hello world!" :generate-overrides-p t)))
    ;; Functions insert-karaoke* free to add any karaoke type without
    ;; care about existing karaoke or override existences
    (insert-karaoke txt1 0 12)
    (insert-karaoke-fill txt1 "lo w" 13)
    (insert-karaoke-outline txt1 "world")
    (is (string= "{\\k12}Hel{\\kf13}lo {\\ko15}world!" (ps-string txt1)))
    (insert-karaoke-outline txt1 0)     ; Changing type without changing value
    (insert-karaoke-fill txt1 3 11)     ; Changing value with same type
    (insert-karaoke txt1 "world" 10)    ; Changing type and value
    (is (string= "{\\ko12}Hel{\\kf11}lo {\\k10}world!" (ps-string txt1)))
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
    (is (= 6 (index (find-override txt1 "world"))))
    (is (string= "\\t(300,400,0.5,\\fax0\\fs18)"
                 (po-string (modifier 'transformation4
                                      :arg4 (list (modifier 'fontshear-x :arg1 0)
                                                  (modifier 'fontsize :arg1 18))))))
    (is (string= "\\1c&H38220C&" (po-string (modifier 'color1 :arg1 (rgb 12 34 56)))))))

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

(test checking-transformation-modifiers
  (let ((str0 "{\\t()}Hello world!")
        (str1 "{\\t(\\clip(573,267,573,267))}Hello world!")
        (str2 "{\\t(3,\\clip(573,267,573,267))}Hello world!")
        (str3 "{\\t(1000,2000,\\clip(573,267,573,267))}Hello world!")
        (str4 "{\\t(1000,2000,3,\\clip(573,267,573,267))}Hello world!"))
    (is (string= str0 (ps-string (text str0))))
    (is (string= "Hello world!" (ps-string (text str0 :remove-unknown-modifier-p t))))
    (is (string= str1 (ps-string (text str1))))
    (is (string= str1 (ps-string (text str1 :remove-unknown-modifier-p t))))
    (is (string= str2 (ps-string (text str2))))
    (is (string= str2 (ps-string (text str2 :remove-unknown-modifier-p t))))
    (is (string= str3 (ps-string (text str3))))
    (is (string= str3 (ps-string (text str3 :remove-unknown-modifier-p t))))
    (is (string= str4 (ps-string (text str4))))
    (is (string= str4 (ps-string (text str4 :remove-unknown-modifier-p t))))))

(test checking-empty-arguments-modifiers
  (let ((str0 "Hello world!")
        (str1 "{\\t()}Hello {\\t( )}world!")
        (str2 "{\\move()}Hello {\\move( )}world!")
        (str3 "{\\pos()}Hello {\\pos( )}world!")
        (str4 "{\\org()}Hello {\\org( )}world!")
        (str5 "{\\fade()}Hello {\\fade( )}world!")
        (str6 "{\\fad()}Hello {\\fad( )}world!")
        (str7 "{\\clip()}Hello {\\clip( )}world!")
        (str8 "{\\iclip()}Hello {\\iclip( )}world!"))
    (is (string= str1 (ps-string (text str1))))
    (is (string= str0 (ps-string (text str1 :remove-unknown-modifier-p t))))
    (is (string= str2 (ps-string (text str2))))
    (is (string= str0 (ps-string (text str2 :remove-unknown-modifier-p t))))
    (is (string= str3 (ps-string (text str3))))
    (is (string= str0 (ps-string (text str3 :remove-unknown-modifier-p t))))
    (is (string= str4 (ps-string (text str4))))
    (is (string= str0 (ps-string (text str4 :remove-unknown-modifier-p t))))
    (is (string= str5 (ps-string (text str5))))
    (is (string= str0 (ps-string (text str5 :remove-unknown-modifier-p t))))
    (is (string= str6 (ps-string (text str6))))
    (is (string= str0 (ps-string (text str6 :remove-unknown-modifier-p t))))
    (is (string= str7 (ps-string (text str7))))
    (is (string= str0 (ps-string (text str7 :remove-unknown-modifier-p t))))
    (is (string= str8 (ps-string (text str8))))
    (is (string= str0 (ps-string (text str8 :remove-unknown-modifier-p t))))))

(test checking-overrides-merging-with-newline
  (let ((txt1 (text "{\\i1}Hello{\\i0}\\Nworld!"))
        (txt2 (text "{\\i1}Hello\\N{\\s1}world!"))
        (txt3 (text "{\\i1}Hello{\\i0}\\N{\\s1}world!"))
        (txt4 (text "{\\i1}Hello\\N{\\i0}{\\s1}world!"))
        (txt5 (text "{\\i1}Hello{\\i0}{\\s1}\\Nworld!"))
        (txt6 (text "{\\i1}Hello{\\N\\N\\i0}\\N\\N{\\s1\\N\\N}world!"))
        (str0 "Helloworld!")
        (str1 "{\\i1}Hello\\N{\\i0}world!")
        (str2 "{\\i1}Hello\\N{\\s1}world!")
        (str3 "{\\i1}Hello\\N{\\i0\\s1}world!"))
    ;; Newline does not reset previous overrides
    ;; No duplication for newline without syllable
    ;; Merge same index overrides and check modifiers duplication
    (is (string= str0 (text txt1)))
    (is (string= str0 (text txt2)))
    (is (string= str0 (text txt3)))
    (is (string= str1 (ps-string txt1)))
    (is (string= str2 (ps-string txt2)))
    (is (string= str3 (ps-string txt3)))
    (is (string= str3 (ps-string txt4)))
    (is (string= str3 (ps-string txt5)))
    (is (string= str3 (ps-string txt6)))))

