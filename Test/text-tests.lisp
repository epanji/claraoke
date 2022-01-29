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

(test checking-string-or-numbers-awareness
  ;; Arguments for modifiers could be a number or a string
  ;; Only number able to be increased or decreased
  (let ((smod01 (modifier 'newline))
        (smod02 (modifier 'bold))
        (smod03 (modifier 'italic))
        (smod04 (modifier 'underline))
        (smod05 (modifier 'strikeout))
        (smod06 (modifier 'border))
        (smod07 (modifier '.shadow))
        (smod08 (modifier 'blur-edges))
        (smod09 (modifier 'fontname))
        (smod10 (modifier 'fontsize))
        (smod11 (modifier 'fontscale-x))
        (smod12 (modifier 'fontscale-y))
        (smod13 (modifier 'fontspace))
        (smod14 (modifier 'fontrotate))
        (smod15 (modifier 'fontrotate-x))
        (smod16 (modifier 'fontrotate-y))
        (smod17 (modifier 'fontrotate-z))
        (smod18 (modifier 'fontencoding))
        (smod19 (modifier 'color))
        (smod20 (modifier 'color1))
        (smod21 (modifier 'color2))
        (smod22 (modifier 'color3))
        (smod23 (modifier 'color4))
        (smod24 (modifier 'alpha))
        (smod25 (modifier 'alpha1))
        (smod26 (modifier 'alpha2))
        (smod27 (modifier 'alpha3))
        (smod28 (modifier 'alpha4))
        (smod29 (modifier 'alignment))
        (smod30 (modifier 'alignment-numpad))
        (smod31 (modifier 'karaoke))
        (smod32 (modifier 'karaoke-fill))
        (smod33 (modifier 'karaoke-outline))
        (smod34 (modifier 'wrapping-style))
        (smod35 (modifier 'reset))
        (smod36 (modifier 'blur))
        (smod37 (modifier 'fontshear-x))
        (smod38 (modifier 'fontshear-y))
        (smod39 (modifier 'border-x))
        (smod40 (modifier 'border-y))
        (smod41 (modifier 'shadow-x))
        (smod42 (modifier 'shadow-y))
        (fmod01 (modifier 'transformation1))
        (fmod02 (modifier 'transformation2))
        (fmod03 (modifier 'transformation3))
        (fmod04 (modifier 'transformation4))
        (fmod05 (modifier 'move))
        (fmod06 (modifier 'pos))
        (fmod07 (modifier 'origin))
        (fmod08 (modifier 'fade))
        (fmod09 (modifier 'fad))
        (fmod10 (modifier 'clip-rectangle))
        (fmod11 (modifier 'clip-drawing))
        (fmod12 (modifier 'clip-drawing-scaled))
        (fmod13 (modifier 'iclip-rectangle))
        (fmod14 (modifier 'iclip-drawing))
        (fmod15 (modifier 'iclip-drawing-scaled))
        (dmod01 (modifier 'drawing-mode))
        (dmod02 (modifier 'drawing-baseline-offset)))
    ;; Style modifiers
    (signals error (increase-modifier smod01))
    (signals error (decrease-modifier smod01))
    (increase-modifier smod02)
    (decrease-modifier smod02)
    (increase-modifier smod03)
    (decrease-modifier smod03)
    (increase-modifier smod04)
    (decrease-modifier smod04)
    (increase-modifier smod05)
    (decrease-modifier smod05)
    (increase-modifier smod06)
    (decrease-modifier smod06)
    (increase-modifier smod07)
    (decrease-modifier smod07)
    (increase-modifier smod08)
    (decrease-modifier smod08)
    (signals error (increase-modifier smod09))
    (signals error (decrease-modifier smod09))
    (increase-modifier smod10)
    (decrease-modifier smod10)
    (increase-modifier smod11)
    (decrease-modifier smod11)
    (increase-modifier smod12)
    (decrease-modifier smod12)
    (increase-modifier smod13)
    (decrease-modifier smod13)
    (increase-modifier smod14)
    (decrease-modifier smod14)
    (increase-modifier smod15)
    (decrease-modifier smod15)
    (increase-modifier smod16)
    (decrease-modifier smod16)
    (increase-modifier smod17)
    (decrease-modifier smod17)
    (increase-modifier smod18)
    (decrease-modifier smod18)
    (signals error (increase-modifier smod19))
    (signals error (decrease-modifier smod19))
    (signals error (increase-modifier smod20))
    (signals error (decrease-modifier smod20))
    (signals error (increase-modifier smod21))
    (signals error (decrease-modifier smod21))
    (signals error (increase-modifier smod22))
    (signals error (decrease-modifier smod22))
    (signals error (increase-modifier smod23))
    (signals error (decrease-modifier smod23))
    (signals error (increase-modifier smod24))
    (signals error (decrease-modifier smod24))
    (signals error (increase-modifier smod25))
    (signals error (decrease-modifier smod25))
    (signals error (increase-modifier smod26))
    (signals error (decrease-modifier smod26))
    (signals error (increase-modifier smod27))
    (signals error (decrease-modifier smod27))
    (signals error (increase-modifier smod28))
    (signals error (decrease-modifier smod28))
    (increase-modifier smod29)
    (decrease-modifier smod29)
    (increase-modifier smod30)
    (decrease-modifier smod30)
    (increase-modifier smod31)
    (decrease-modifier smod31)
    (increase-modifier smod32)
    (decrease-modifier smod32)
    (increase-modifier smod33)
    (decrease-modifier smod33)
    (increase-modifier smod34)
    (decrease-modifier smod34)
    (signals error (increase-modifier smod35))
    (signals error (decrease-modifier smod35))
    (increase-modifier smod36)
    (decrease-modifier smod36)
    (increase-modifier smod37)
    (decrease-modifier smod37)
    (increase-modifier smod38)
    (decrease-modifier smod38)
    (increase-modifier smod39)
    (decrease-modifier smod39)
    (increase-modifier smod40)
    (decrease-modifier smod40)
    (increase-modifier smod41)
    (decrease-modifier smod41)
    (increase-modifier smod42)
    (decrease-modifier smod42)
    ;; Tranformation1
    (signals error (increase-modifier fmod01))
    (signals error (decrease-modifier fmod01))
    ;; Tranformation2
    (increase-modifier fmod02 1)
    (decrease-modifier fmod02 1)
    (signals error (increase-modifier fmod02 1 :arg2))
    (signals error (decrease-modifier fmod02 1 :arg2))
    ;; Tranformation3
    (increase-modifier fmod03 1)
    (decrease-modifier fmod03 1)
    (increase-modifier fmod03 1 :arg2)
    (decrease-modifier fmod03 1 :arg2)
    (signals error (increase-modifier fmod03 1 :arg3))
    (signals error (decrease-modifier fmod03 1 :arg3))
    ;; Tranformation4
    (increase-modifier fmod04 1)
    (decrease-modifier fmod04 1)
    (increase-modifier fmod04 1 :arg2)
    (decrease-modifier fmod04 1 :arg2)
    (increase-modifier fmod04 1 :arg3)
    (decrease-modifier fmod04 1 :arg3)
    (signals error (increase-modifier fmod04 1 :arg4))
    (signals error (decrease-modifier fmod04 1 :arg4))
    ;; Move
    (increase-modifier fmod05 1)
    (decrease-modifier fmod05 1)
    (increase-modifier fmod05 1 :arg2)
    (decrease-modifier fmod05 1 :arg2)
    (increase-modifier fmod05 1 :arg3)
    (decrease-modifier fmod05 1 :arg3)
    (increase-modifier fmod05 1 :arg4)
    (decrease-modifier fmod05 1 :arg4)
    ;; Pos
    (increase-modifier fmod06 1)
    (decrease-modifier fmod06 1)
    (increase-modifier fmod06 1 :arg2)
    (decrease-modifier fmod06 1 :arg2)
    ;; Origin
    (increase-modifier fmod07 1)
    (decrease-modifier fmod07 1)
    (increase-modifier fmod07 1 :arg2)
    (decrease-modifier fmod07 1 :arg2)
    ;; Fade
    (increase-modifier fmod08 1)
    (decrease-modifier fmod08 1)
    (increase-modifier fmod08 1 :arg2)
    (decrease-modifier fmod08 1 :arg2)
    (increase-modifier fmod08 1 :arg3)
    (decrease-modifier fmod08 1 :arg3)
    (increase-modifier fmod08 1 :arg4)
    (decrease-modifier fmod08 1 :arg4)
    (increase-modifier fmod08 1 :arg5)
    (decrease-modifier fmod08 1 :arg5)
    (increase-modifier fmod08 1 :arg6)
    (decrease-modifier fmod08 1 :arg6)
    (increase-modifier fmod08 1 :arg7)
    (decrease-modifier fmod08 1 :arg7)
    ;; Fad
    (increase-modifier fmod09 1)
    (decrease-modifier fmod09 1)
    (increase-modifier fmod09 1 :arg2)
    (decrease-modifier fmod09 1 :arg2)
    ;; Clip rectangle
    (increase-modifier fmod10 1)
    (decrease-modifier fmod10 1)
    (increase-modifier fmod10 1 :arg2)
    (decrease-modifier fmod10 1 :arg2)
    (increase-modifier fmod10 1 :arg3)
    (decrease-modifier fmod10 1 :arg3)
    (increase-modifier fmod10 1 :arg4)
    (decrease-modifier fmod10 1 :arg4)
    ;; Clip drawing
    (signals error (increase-modifier fmod11 1))
    (signals error (decrease-modifier fmod11 1))
    ;; Clip drawing scaled
    (increase-modifier fmod12 1)
    (decrease-modifier fmod12 1)
    (signals error (increase-modifier fmod12 1 :arg2))
    (signals error (decrease-modifier fmod12 1 :arg2))
    ;; iClip rectangle
    (increase-modifier fmod13 1)
    (decrease-modifier fmod13 1)
    (increase-modifier fmod13 1 :arg2)
    (decrease-modifier fmod13 1 :arg2)
    (increase-modifier fmod13 1 :arg3)
    (decrease-modifier fmod13 1 :arg3)
    (increase-modifier fmod13 1 :arg4)
    (decrease-modifier fmod13 1 :arg4)
    ;; iClip drawing
    (signals error (increase-modifier fmod14 1))
    (signals error (decrease-modifier fmod14 1))
    ;; iClip drawing scaled
    (increase-modifier fmod15 1)
    (decrease-modifier fmod15 1)
    (signals error (increase-modifier fmod15 1 :arg2))
    (signals error (decrease-modifier fmod15 1 :arg2))
    ;; Drawing mode
    (increase-modifier dmod01 1)
    (decrease-modifier dmod01 1)
    ;; Drawing baseline offset
    (increase-modifier dmod02 1)
    (decrease-modifier dmod02 1)))

(test checking-overrides-generations
  (let* ((str0 "Hello world!")
         (str1 "こんにちは世界！")
         (str2 "你好世界！")
         (str3 "Jangan lupa 快乐的 setiap hari.")
         (str4 "⁣There is invisible separator.")
         (txt0 (text str0 :generate-overrides-p t :spell-duration 15))
         (txt1 (text str1 :generate-overrides-p t :spell-duration 15))
         (txt2 (text str2 :generate-overrides-p t :spell-duration 15))
         (txt3 (text str3 :generate-overrides-p t :spell-duration 15))
         (txt4 (text str4 :generate-overrides-p t :spell-duration 15)))
    (is (string= "{\\k15}Hel{\\k15}lo {\\k15}world!" (ps-string txt0)))
    (is (string= "{\\k15}こ{\\k15}ん{\\k15}に{\\k15}ち{\\k15}は{\\k15}世{\\k15}界{\\k15}！" (ps-string txt1)))
    (is (string= "{\\k15}你{\\k15}好{\\k15}世{\\k15}界{\\k15}！" (ps-string txt2)))
    (is (string= "{\\k15}Ja{\\k15}ngan {\\k15}lu{\\k15}pa {\\k15}快{\\k15}乐{\\k15}的 {\\k15}se{\\k30}tiap {\\k15}ha{\\k15}ri." (ps-string txt3)))
    (is (string= "{\\k15}⁣{\\k15}The{\\k15}re {\\k15}is {\\k15}in{\\k15}vi{\\k15}si{\\k15}ble {\\k15}se{\\k15}pa{\\k15}ra{\\k15}tor." (ps-string txt4)))))

