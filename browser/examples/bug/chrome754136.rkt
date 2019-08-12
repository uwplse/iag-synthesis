;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome754136.html

(define-stylesheet doc-1
  ((class container)
   [width (px 300)]
   #;[border-top-color black]
   [border-top-style solid]
   [border-top-width (px 3)]
   #;[border-right-color black]
   [border-right-style solid]
   [border-right-width (px 3)]
   #;[border-bottom-color black]
   [border-bottom-style solid]
   [border-bottom-width (px 3)]
   #;[border-left-color black]
   [border-left-style solid]
   [border-left-width (px 3)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1])
  ((class padded)
   [padding-right (px 260)]
   [height (px 50)]
   #;[border-top-color lime]
   [border-top-style dotted]
   [border-top-width (px 2)]
   #;[border-right-color lime]
   [border-right-style dotted]
   [border-right-width (px 2)]
   #;[border-bottom-color lime]
   [border-bottom-style dotted]
   [border-bottom-width (px 2)]
   #;[border-left-color lime]
   [border-left-style dotted]
   [border-left-width (px 2)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1])
  ((class float)
   [float right]
   [width (px 50)]
   [height (px 50)]
   #;[border-top-color red]
   [border-top-style dotted]
   [border-top-width (px 2)]
   #;[border-right-color red]
   [border-right-style dotted]
   [border-right-width (px 2)]
   #;[border-bottom-color red]
   [border-bottom-style dotted]
   [border-bottom-width (px 2)]
   #;[border-left-color red]
   [border-left-style dotted]
   [border-left-width (px 2)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 65 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 25.2 :elt 4]
    ([BLOCK :x 8 :y 8 :w 306 :h 25.2 :elt 5]
     ([LINE]
      ([INLINE :elt 6]
       ([BLOCK :x 257 :y 11 :w 54 :h 54 :elt 7]))))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([div :num 5 :class (container)]
     ([span :num 6 :class (padded)] " "
      ([span :num 7 :class (float)]) " ")) " ")))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome754136.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:1)

