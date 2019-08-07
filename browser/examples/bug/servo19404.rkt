;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo19404.html

(define-stylesheet doc-1
  ((class right)
   [float right]
   [width (px 700)]
   [background-color lightblue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((class content)
   [overflow-x auto]
   [overflow-y auto]
   [width (px 700)]
   [background-color lightgrey]
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
  ([BLOCK :x 0 :y 0 :w 1280 :h 16 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3]
    ([BLOCK :x 572 :y 8 :w 700 :h 0 :elt 4])
    ([BLOCK :x 8 :y 8 :w 700 :h 0 :elt 5])))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :class (right)])
    ([div :num 5 :class (content)]) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo19404.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float css:overflow-x overflow:auto css:overflow-y float:1)

