;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-collapse-009.html

(define-stylesheet doc-1
  ((tag div)
   [height (px 5)]
   [width (px 5)])
  ((id div1)
   [height (px 2)]
   [margin-top (px 2)]
   [overflow-x hidden]
   [overflow-y hidden])
  ((id div2)
   [height (px 1)]
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [margin-top (px 2)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 21 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 5 :elt 3]
    ([BLOCK :x 8 :y 8 :w 5 :h 5 :elt 4]
     ([BLOCK :x 8 :y 8 :w 5 :h 2 :elt 5]
      ([BLOCK :x 8 :y 10 :w 5 :h 1 :elt 6])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4]
     ([div :num 5 :id div1]
      ([div :num 6 :id div2]))) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-collapse-009.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:overflow-x css:overflow-y float:0)

