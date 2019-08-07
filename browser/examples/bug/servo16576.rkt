;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo16576.html

(define-stylesheet doc-1
  ((id box)
   [background-color (rgb 8 11 12)]
   [height (px 100)]
   [overflow-x hidden]
   [overflow-y hidden]
   [padding-top (px 0)]
   [padding-right (px 15)]
   [padding-bottom (px 0)]
   [padding-left (px 15)]
   [position relative])
  ((id inside)
   [position absolute]
   [right (px 0)]
   [width (px 100)]
   [height (% 100)]
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 116 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 100 :elt 3]
    ([BLOCK :x 8 :y 8 :w 1264 :h 100 :elt 4]
     ([BLOCK :x 1172 :y 8 :w 100 :h 100 :elt 5]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id box]
     ([div :num 5 :id inside])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo16576.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:overflow-x css:overflow-y css:position float:0)

