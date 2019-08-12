;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome815311.html

(define-stylesheet doc-1
  ((id container)
   [width (px 300)]
   #;[border-top-color black]
   [border-top-style solid]
   [border-top-width (px 1)]
   #;[border-right-color black]
   [border-right-style solid]
   [border-right-width (px 1)]
   #;[border-bottom-color black]
   [border-bottom-style solid]
   [border-bottom-width (px 1)]
   #;[border-left-color black]
   [border-left-style solid]
   [border-left-width (px 1)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1])
  ((id left)
   [width (px 20)]
   [height (px 20)]
   [float left]
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id right)
   [width (px 20)]
   [height (px 20)]
   [float right]
   [background-color yellow]
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
  ([BLOCK :x 0 :y 0 :w 1280 :h 29 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 2 :elt 4]
    ([BLOCK :x 8 :y 8 :w 302 :h 2 :elt 5]
     ([BLOCK :x 9 :y 9 :w 20 :h 20 :elt 6])
     ([BLOCK :x 289 :y 9 :w 20 :h 20 :elt 7]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([div :num 5 :id container]
     ([div :num 6 :id left]) " "
     ([div :num 7 :id right])) " ")))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome815311.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:1)

