;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-pavel.html

(define-stylesheet doc-1
  ((tag div)
   [margin-top (px 100)]
   [margin-bottom (px 100)])
  ((id box1)
   [height (px 10)]
   [background-color lightblue])
  ((id box2)
   [background-color pink])
  ((id box3)
   [height (px 0)]
   [margin-left (px 100)]
   [background-color yellow])
  ((id box4)
   [height (px 10)]
   [margin-top (px 200)]
   [margin-left (px 100)]
   [background-color green]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 420 :elt 0]
   ([BLOCK :x 8 :y 100 :w 1264 :h 220 :elt 3]
    ([BLOCK :x 8 :y 100 :w 1264 :h 10 :elt 4])
    ([BLOCK :x 8 :y 310 :w 1264 :h 10 :elt 5]
     ([BLOCK :x 108 :y 310 :w 1164 :h 0 :elt 6])
     ([BLOCK :x 108 :y 310 :w 1164 :h 10 :elt 7]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id box1])
    ([div :num 5 :id box2]
     ([div :num 6 :id box3])
     ([div :num 7 :id box4])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-pavel.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features float:0)

