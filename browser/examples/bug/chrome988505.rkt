;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome988505.html

(define-stylesheet doc-1
  ((id container)
   [width (px 0)])
  ((id breadcrumbs)
   [float left]
   [height (px 20)]
   [width (px 20)]
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id content)
   [float left]
   [height (px 20)]
   [width (px 20)]
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

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 48 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3]
    ([BLOCK :x 8 :y 8 :w 0 :h 0 :elt 4]
     ([BLOCK :x 8 :y 8 :w 0 :h 0 :elt 5]
      ([BLOCK :x 8 :y 8 :w 20 :h 20 :elt 6])
      ([BLOCK :x 8 :y 28 :w 20 :h 20 :elt 7])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id container]
     ([div :num 5]
      ([div :num 6 :id breadcrumbs]) " "
      ([div :num 7 :id content]))) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome988505.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:2)

