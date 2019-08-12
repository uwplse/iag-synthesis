;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/webkit18080.html

(define-stylesheet doc-1
  ((id A)
   [background-color (rgb 61 127 175)]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [height (px 36)])
  ((id d1)
   [overflow-x hidden]
   [overflow-y hidden])
  ((id d2)
   [width (px 10)]
   [height (px 10)]
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [margin-top (px 8)]
   [margin-right (px 8)]
   [margin-bottom (px 8)]
   [margin-left (px 8)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 52 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 36 :elt 3]
    ([BLOCK :x 8 :y 8 :w 1264 :h 36 :elt 4]
     ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 5])
     ([BLOCK :x 16 :y 16 :w 10 :h 10 :elt 6]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id A]
     ([div :num 5 :id d1])
     ([div :num 6 :id d2])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/webkit18080.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:overflow-x css:overflow-y float:0)

