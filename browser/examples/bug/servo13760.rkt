;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13760.html

(define-stylesheet doc-1
  ((id a)
   [height (px 200)]
   [width (px 600)]
   [background-color gold]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id b)
   [position relative]
   [top (% 50)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 216 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 200 :elt 3]
    ([BLOCK :x 8 :y 8 :w 600 :h 200 :elt 4]
     ([BLOCK :x 8 :y 108 :w 600 :h 0 :elt 5]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id a]
     ([div :num 5 :id b])))))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13760.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:position float:0)

