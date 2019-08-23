;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome530964.html

(define-stylesheet doc-1
  ((class container)
   #;[border-top-color red]
   [border-top-style solid]
   [border-top-width (px 1)]
   #;[border-right-color red]
   [border-right-style solid]
   [border-right-width (px 1)]
   #;[border-bottom-color red]
   [border-bottom-style solid]
   [border-bottom-width (px 1)]
   #;[border-left-color red]
   [border-left-style solid]
   [border-left-width (px 1)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1]
   [width (px 300)]
   [height (px 800)])
  ((class row)
   #;[border-bottom-color black]
   [border-bottom-style solid]
   [border-bottom-width (px 1)]
   [width (px 300)]
   [height (px 66.6666)]
   [float left]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 816 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 800 :elt 4]
    ([BLOCK :x 8 :y 8 :w 300 :h 800 :elt 5]
     ([BLOCK :x 9 :y 9 :w 298 :h (/ 200 3) :elt 6])
     ([BLOCK :x 9 :y (/ 227 3) :w 298 :h (/ 200 3) :elt 7])
     ([BLOCK :x 9 :y (/ 427 3) :w 298 :h (/ 200 3) :elt 8])
     ([BLOCK :x 9 :y 209 :w 298 :h (/ 200 3) :elt 9])
     ([BLOCK :x 9 :y (/ 827 3) :w 298 :h (/ 200 3) :elt 10])
     ([BLOCK :x 9 :y (/ 1027 3) :w 298 :h (/ 200 3) :elt 11])
     ([BLOCK :x 9 :y 409 :w 298 :h (/ 200 3) :elt 12])
     ([BLOCK :x 9 :y (/ 1427 3) :w 298 :h (/ 200 3) :elt 13])
     ([BLOCK :x 9 :y (/ 1627 3) :w 298 :h (/ 200 3) :elt 14])
     ([BLOCK :x 9 :y 609 :w 298 :h (/ 200 3) :elt 15])
     ([BLOCK :x 9 :y (/ 2027 3) :w 298 :h (/ 200 3) :elt 16])
     ([BLOCK :x 9 :y (/ 2227 3) :w 298 :h (/ 200 3) :elt 17]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([div :num 5 :class (container)]
     ([div :num 6 :class (row)]) " "
     ([div :num 7 :class (row)]) " "
     ([div :num 8 :class (row)]) " "
     ([div :num 9 :class (row)]) " "
     ([div :num 10 :class (row)]) " "
     ([div :num 11 :class (row)]) " "
     ([div :num 12 :class (row)]) " "
     ([div :num 13 :class (row)]) " "
     ([div :num 14 :class (row)]) " "
     ([div :num 15 :class (row)]) " "
     ([div :num 16 :class (row)]) " "
     ([div :num 17 :class (row)])) " ")))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome530964.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:box-sizing css:float empty-text float:2)

