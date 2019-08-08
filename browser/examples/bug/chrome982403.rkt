;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome982403.html

(define-stylesheet doc-1
  ((id div1)
   [display flow-root])
  ((id div2)
   [float left]
   [width (px 100)]
   [height (px 50)]
   [background-color (rgb 0 0 255)])
  ((id div3)
   [clear both]
   [width (px 100)]
   [height (px 50)])
  ((id div4)
   [position absolute]
   [display inline]
   [width (px 100)]
   [height (px 50)]
   [background-color blue]
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

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 116 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 100 :elt 3]
    ([MAGIC :x 8 :y 8 :w 1264 :h 100 :elt 4]
     ([BLOCK :x 8 :y 8 :w 100 :h 50 :elt 5])
     ([BLOCK :x 8 :y 58 :w 100 :h 50 :elt 6]
      ([BLOCK :x 8 :y 58 :w 100 :h 50 :elt 7])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id div1]
     ([div :num 5 :id div2])
     ([div :num 6 :id div3]
      ([div :num 7 :id div4]))) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome982403.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float css:clear css:position display:unknown float:1)

