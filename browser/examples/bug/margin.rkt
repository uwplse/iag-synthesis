;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/margin.html

(define-stylesheet doc-1
  (*
   [display block]
   [margin-top (px 0)]
   [margin-right (px 0)]
   [margin-bottom (px 0)]
   [margin-left (px 0)]
   [padding-top (px 0)]
   [padding-right (px 0)]
   [padding-bottom (px 0)]
   [padding-left (px 0)])
  ((tag html)
   [width (px 600)]
   [margin-top auto]
   [margin-right auto]
   [margin-bottom auto]
   [margin-left auto]
   [padding-top (px 10)]
   [padding-right (px 10)]
   [padding-bottom (px 10)]
   [padding-left (px 10)])
  ((tag head)
   [display none])
  ((tag div)
   [border-top-width (px 1)]
   [border-right-width (px 1)]
   [border-bottom-width (px 1)]
   [border-left-width (px 1)]
   [border-top-style solid]
   [border-right-style solid]
   [border-bottom-style solid]
   [border-left-style solid]
   #;[border-top-color (rgb 0 0 0)]
   #;[border-right-color (rgb 0 0 0)]
   #;[border-bottom-color (rgb 0 0 0)]
   #;[border-left-color (rgb 0 0 0)])
  ((class float)
   [float left]
   [height (px 100)]
   [width (px 100)])
  ((id upper)
   [background-color (rgb 0 0 255)])
  ((id middle)
   [height (px 85)]
   [margin-bottom (px 20)]
   [background-color (rgb 0 255 0)])
  ((id lower)
   [background-color (rgb 255 0 0)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 330 :y 0 :w 620 :h 229 :elt 0]
   ([BLOCK :x 340 :y 10 :w 600 :h 87 :elt 3]
    ([BLOCK :x 340 :y 10 :w 102 :h 102 :elt 4])
    ([BLOCK :x 340 :y 10 :w 600 :h 87 :elt 5])
    ([BLOCK :x 340 :y 117 :w 102 :h 102 :elt 6])))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id upper :class (float)])
    ([div :num 5 :id middle])
    ([div :num 6 :id lower :class (float)]) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/margin.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float float:1)

