;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/float-006.html

(define-stylesheet doc-1
  ((id rel-pos-containing-block)
   [position relative])
  ((id green-overlapping-abs-pos)
   [background-color green]
   #;[border-top-color green]
   #;[border-right-color green]
   #;[border-bottom-color green]
   #;[border-left-color green]
   [border-top-style solid]
   [border-right-style solid]
   [border-bottom-style solid]
   [border-left-style solid]
   [border-left-width (px 3)]
   [border-right-width (px 3)]
   [border-top-width (px 32)]
   [border-bottom-width (px 32)]
   [left (px 0)]
   [padding-top (px 48)]
   [padding-right (px 48)]
   [padding-bottom (px 48)]
   [padding-left (px 48)]
   [position absolute]
   [width (px 128)]
   #;[z-index auto])
  ((id zero-height-first-float)
   [float left]
   [width (px 288)])
  ((id red-overlapped-second-float)
   [background-color red]
   [float left]
   [padding-top (px 80)]
   [padding-right (px 80)]
   [padding-bottom (px 80)]
   [padding-left (px 80)]
   [width (px 64)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 168 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3]
    ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 4]
     ([BLOCK :x 8 :y 8 :w 230 :h 160 :elt 5])
     ([BLOCK :x 8 :y 8 :w 288 :h 0 :elt 6])
     ([BLOCK :x 8 :y 8 :w 224 :h 160 :elt 7]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id rel-pos-containing-block]
     ([div :num 5 :id green-overlapping-abs-pos]) " "
     ([div :num 6 :id zero-height-first-float]) " "
     ([div :num 7 :id red-overlapped-second-float])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/float-006.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:position css:float empty-text float:2)

