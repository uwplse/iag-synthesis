;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome810370.html

(define-stylesheet doc-1
  ((tag div)
   [width (px 30)]
   [height (px 90)]
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id d0)
   [position absolute])
  ((id d1)
   [float left])
  ((id d2)
   [display inline-block])
  ((id d3)
   [float left]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 8 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 4]
    ([BLOCK :x 8 :y 8 :w 30 :h 90 :elt 5]
     ([BLOCK :x 8 :y 8 :w 30 :h 90 :elt 6])
     ([BLOCK :x 8 :y 8 :w 30 :h 90 :elt 7]
      ([LINE]
       ([INLINE :x 8 :y 98 :w 30 :h 90 :elt 8])
       ;([TEXT :x 38 :y 176 :w 0 :h 16 :text " "])
       ([BLOCK :x 8 :y 193.6 :w 30 :h 90 :elt 9]))))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([div :num 5 :id d0]
     ([div :num 6 :id d1])
     ([div :num 7]
      ([div :num 8 :id d2])
      ([div :num 9 :id d3]))))))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome810370.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:position css:float display:inline-block float:1)

