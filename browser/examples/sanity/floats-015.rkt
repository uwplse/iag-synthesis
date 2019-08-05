;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-015.html

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
   [height (px 232)]
   [width (px 232)])
  ((desc (tag div) (tag div))
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [height (px 96)]
   [margin-top (px 10)]
   [margin-right (px 10)]
   [margin-bottom (px 10)]
   [margin-left (px 10)]
   [width (px 96)])
  ((id div2)
   [float right])
  ((id div4)
   [float right])
  ((id div3)
   [float left]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 258 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 242 :elt 9]
    ([BLOCK :x 8 :y 8 :w 242 :h 242 :elt 10]
     ([BLOCK :x 139 :y 23 :w 96 :h 96 :elt 11])
     ([BLOCK :x 23 :y 23 :w 96 :h 96 :elt 12])
     ([BLOCK :x 23 :y 139 :w 96 :h 96 :elt 13])
     ([BLOCK :x 139 :y 139 :w 96 :h 96 :elt 14]))))))

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
     ([div :num 11 :id div2])
     ([div :num 12])
     ([div :num 13 :id div3]) " "
     ([div :num 14 :id div4])) " ")))

(define-problem doc-1
  :title "CSS Test: Right floated elements margins do not collapse"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-015.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:2)

