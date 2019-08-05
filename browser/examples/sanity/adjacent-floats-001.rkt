;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/adjacent-floats-001.html

(define-stylesheet doc-1
  ((desc (tag div) (tag div))
   [float left]
   [height (px 96)]
   [width (px 96)])
  ((id div1)
   [background-color orange])
  ((id div2)
   [background-color blue]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 104 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 9]
    ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 10]
     ([BLOCK :x 8 :y 8 :w 96 :h 96 :elt 11])
     ([BLOCK :x 104 :y 8 :w 96 :h 96 :elt 12]))))))

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
    ([div :num 10]
     ([div :num 11 :id div1]) " "
     ([div :num 12 :id div2])) " ")))

(define-problem doc-1
  :title "CSS Test: Multiple floated boxes adjacent to each other"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/adjacent-floats-001.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:1)

