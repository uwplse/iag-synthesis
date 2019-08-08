;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/float-pavel.html

(define-stylesheet doc-1
  ((id d1)
   [float left]
   [background-color pink]
   [height (px 100)]
   [width (px 500)])
  ((id d2)
   [float right]
   [background-color pink]
   [height (px 100)]
   [width (px 500)])
  ((id d2)
   [float right]
   [background-color lightblue]
   [height (px 100)]
   [width (px 500)])
  ((id d3)
   [float left]
   [background-color yellow]
   [height (px 100)]
   [width (px 4000)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 208 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3]
    ([BLOCK :x 8 :y 8 :w 500 :h 100 :elt 4])
    ([BLOCK :x 772 :y 8 :w 500 :h 100 :elt 5])
    ([BLOCK :x 8 :y 108 :w 4000 :h 100 :elt 6])))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id d1]) " "
    ([div :num 5 :id d2]) " "
    ([div :num 6 :id d3]) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/float-pavel.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:2)

