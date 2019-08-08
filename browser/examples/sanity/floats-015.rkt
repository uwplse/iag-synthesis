;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-015.html

(define-stylesheet doc-1
  ((id div1)
   [border-top-style solid]
   [border-right-style solid]
   [border-bottom-style solid]
   [border-left-style solid]
   [border-top-width (px 5)]
   [border-right-width (px 5)]
   [border-bottom-width (px 5)]
   [border-left-width (px 5)]
   #;[border-top-color black]
   #;[border-right-color black]
   #;[border-bottom-color black]
   #;[border-left-color black]
   [height (px 232)]
   [width (px 232)])
  ((desc (tag div) (tag div))
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [height (px 96)]
   [margin-top (px 10)]
   [margin-right (px 10)]
   [margin-bottom (px 10)]
   [margin-left (px 10)]
   [width (px 96)])
  ((id div2)
   [float right])
  ((id div4)
   [float right])
  ((id div3)
   [float left]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 258 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 242 :elt 3]
    ([BLOCK :x 8 :y 8 :w 242 :h 242 :elt 4]
     ([BLOCK :x 139 :y 23 :w 96 :h 96 :elt 5])
     ([BLOCK :x 23 :y 23 :w 96 :h 96 :elt 6])
     ([BLOCK :x 23 :y 139 :w 96 :h 96 :elt 7])
     ([BLOCK :x 139 :y 139 :w 96 :h 96 :elt 8]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id div1]
     ([div :num 5 :id div2])
     ([div :num 6])
     ([div :num 7 :id div3]) " "
     ([div :num 8 :id div4])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-015.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:2)

