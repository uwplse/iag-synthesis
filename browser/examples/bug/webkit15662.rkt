;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/webkit15662.html

(define-stylesheet doc-1
  ((id d1)
   [width (px 982)]
   [height (px 383)]
   #;[border-top-color red]
   [border-top-style solid]
   [border-top-width (px 2)]
   #;[border-right-color red]
   [border-right-style solid]
   [border-right-width (px 2)]
   #;[border-bottom-color red]
   [border-bottom-style solid]
   [border-bottom-width (px 2)]
   #;[border-left-color red]
   [border-left-style solid]
   [border-left-width (px 2)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1])
  ((id d2)
   [float left]
   #;[border-top-color blue]
   [border-top-style solid]
   [border-top-width (px 2)]
   #;[border-right-color blue]
   [border-right-style solid]
   [border-right-width (px 2)]
   #;[border-bottom-color blue]
   [border-bottom-style solid]
   [border-bottom-width (px 2)]
   #;[border-left-color blue]
   [border-left-style solid]
   [border-left-width (px 2)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1])
  ((id d3)
   [float left]
   [width (px 615)]
   [height (px 191)]
   #;[border-top-color green]
   [border-top-style solid]
   [border-top-width (px 2)]
   #;[border-right-color green]
   [border-right-style solid]
   [border-right-width (px 2)]
   #;[border-bottom-color green]
   [border-bottom-style solid]
   [border-bottom-width (px 2)]
   #;[border-left-color green]
   [border-left-style solid]
   [border-left-width (px 2)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1])
  ((id d4)
   [float left]
   [width (px 305)]
   [height (px 230)]
   #;[border-top-color red]
   [border-top-style dotted]
   [border-top-width (px 1)]
   #;[border-right-color red]
   [border-right-style dotted]
   [border-right-width (px 1)]
   #;[border-bottom-color red]
   [border-bottom-style dotted]
   [border-bottom-width (px 1)]
   #;[border-left-color red]
   [border-left-style dotted]
   [border-left-width (px 1)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1])
  ((id d5)
   [float left]
   [width (px 268)]
   [height (px 113)]
   #;[border-top-color black]
   [border-top-style solid]
   [border-top-width (px 2)]
   #;[border-right-color black]
   [border-right-style solid]
   [border-right-width (px 2)]
   #;[border-bottom-color black]
   [border-bottom-style solid]
   [border-bottom-width (px 2)]
   #;[border-left-color black]
   [border-left-style solid]
   [border-left-width (px 2)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 403 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 387 :elt 3]
    ([BLOCK :x 8 :y 8 :w 986 :h 387 :elt 4]
     ([BLOCK :x 10 :y 10 :w 982 :h 353 :elt 5]
      ([BLOCK :x 12 :y 12 :w 619 :h 195 :elt 6])
      ([BLOCK :x 631 :y 12 :w 307 :h 232 :elt 7])
      ([BLOCK :x 12 :y 244 :w 272 :h 117 :elt 8])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id d1]
     ([div :num 5 :id d2]
      ([div :num 6 :id d3]) " "
      ([div :num 7 :id d4]) " "
      ([div :num 8 :id d5]))) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/webkit15662.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:2)

