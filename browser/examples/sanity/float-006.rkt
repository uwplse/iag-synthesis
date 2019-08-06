;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/float-006.html

(define-stylesheet doc-1
  ((id rel-pos-containing-block)
   [position relative])
  ((id green-overlapping-abs-pos)
   [background-color green]
   #;[border-top-color green]
   [border-top-style solid]
   [border-top-width (em 2)]
   #;[border-bottom-color green]
   [border-bottom-style solid]
   [border-bottom-width (em 2)]
   [left (em 0)]
   [padding-top (em 3)]
   [padding-right (em 3)]
   [padding-bottom (em 3)]
   [padding-left (em 3)]
   [position absolute]
   [width (em 8)]
   #;[z-index auto])
  ((id zero-height-first-float)
   [float left]
   [width (em 18)])
  ((id red-overlapped-second-float)
   [background-color red]
   [float left]
   [padding-top (em 5)]
   [padding-right (em 5)]
   [padding-bottom (em 5)]
   [padding-left (em 5)]
   [width (em 4)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 168 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3]
    ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 4]
     ([BLOCK :x 8 :y 8 :w 224 :h 160 :elt 5])
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

