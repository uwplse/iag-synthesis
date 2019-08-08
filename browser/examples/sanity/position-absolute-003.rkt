;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/position-absolute-003.html

(define-stylesheet doc-1
  ((id div1)
   [height (px 96)]
   [position relative]
   [width (px 192)])
  ((id div2)
   [height (px 96)]
   [width (px 96)])
  ((id div3)
   [height (px 96)]
   [width (px 96)])
  ((id div2)
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id div3)
   [background-color green]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [position absolute]
   [left (px 0)]
   [top (px 0)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 112 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 96 :elt 3]
    ([BLOCK :x 8 :y 8 :w 192 :h 96 :elt 4]
     ([BLOCK :x 8 :y 8 :w 96 :h 96 :elt 5])
     ([BLOCK :x 8 :y 8 :w 96 :h 96 :elt 6]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id div1]
     ([div :num 5 :id div2])
     ([div :num 6 :id div3])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/position-absolute-003.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:position float:0)

