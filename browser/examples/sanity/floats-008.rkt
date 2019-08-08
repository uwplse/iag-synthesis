;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-008.html

(define-stylesheet doc-1
  ((tag div)
   [height (px 96)]
   [width (px 96)])
  ((id div1)
   [background-color orange])
  ((id div2)
   [background-color blue]
   [float left])
  ((id div3)
   [background-color red]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 208 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 192 :elt 3]
    ([BLOCK :x 8 :y 8 :w 96 :h 96 :elt 4])
    ([BLOCK :x 8 :y 104 :w 96 :h 96 :elt 5])
    ([BLOCK :x 8 :y 104 :w 96 :h 96 :elt 6])))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id div1])
    ([div :num 5 :id div2])
    ([div :num 6 :id div3]) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-008.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float float:1)

