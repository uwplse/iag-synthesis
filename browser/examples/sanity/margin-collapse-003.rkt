;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-collapse-003.html

(define-stylesheet doc-1
  ((desc (tag div) (tag div))
   [height (px 20)]
   [width (px 50)])
  ((id div1)
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [margin-bottom (px 192)])
  ((id div2)
   [background-color orange]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [margin-top (px -192)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 56 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 40 :elt 3]
    ([BLOCK :x 8 :y 8 :w 1264 :h 40 :elt 4]
     ([BLOCK :x 8 :y 8 :w 50 :h 20 :elt 5])
     ([BLOCK :x 8 :y 28 :w 50 :h 20 :elt 6]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4]
     ([div :num 5 :id div1])
     ([div :num 6 :id div2])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-collapse-003.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features float:0)

