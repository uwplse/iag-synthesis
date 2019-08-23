;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13683.html

(define-stylesheet doc-1
  ((tag div)
   [float left]
   [width (px 40)]
   [height (px 40)]
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((tag span)
   [color black]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 48 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3]
    ([LINE]
     ([INLINE :elt 4]
      ([BLOCK :x 8 :y 8 :w 40 :h 40 :elt 5])
      ([BLOCK :x 48 :y 8 :w 40 :h 40 :elt 6]))
     ([BLOCK :x 88 :y 8 :w 40 :h 40 :elt 7]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([span :num 4]
     ([div :num 5])
     ([div :num 6])) " "
    ([div :num 7]) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13683.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:1)

