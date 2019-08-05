;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-002.html

(define-stylesheet doc-1
  ((id div1)
   #;[border-top-color black]
   [border-top-style solid]
   [border-top-width (px 5)]
   #;[border-right-color black]
   [border-right-style solid]
   [border-right-width (px 5)]
   #;[border-bottom-color black]
   [border-bottom-style solid]
   [border-bottom-width (px 5)]
   #;[border-left-color black]
   [border-left-style solid]
   [border-left-width (px 5)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1]
   [height (px 96)]
   [width (px 192)])
  ((desc (tag div) (tag div))
   [background-color blue]
   [height (px 96)]
   [float right]
   [width (px 96)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 122 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 106 :elt 9]
    ([BLOCK :x 8 :y 8 :w 202 :h 106 :elt 10]
     ([BLOCK :x 109 :y 13 :w 96 :h 96 :elt 11]))))))

(define-document doc-1
  ([html :num 0 :class (gr__test_csswg_org)]
   ([head :num 1]
    ([meta :num 2])
    ([title :num 3])
    ([link :num 4])
    ([link :num 5])
    ([meta :num 6])
    ([meta :num 7])
    ([style :num 8]))
   ([body :num 9]
    ([div :num 10 :id div1]
     ([div :num 11])) " ")))

(define-problem doc-1
  :title "CSS Test: Floats, shifting right until it touches container edge"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-002.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float float:1)

