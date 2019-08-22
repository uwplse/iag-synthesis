;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome875160.html

(define-stylesheet doc-1
  ((tag float)
   [float left]
   [width (px 100)]
   [height (px 100)])
  ((id f1)
   [background-color orange]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [width (px 100)])
  ((id f2)
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [margin-top (px -80)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 108 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 4]
    ([BLOCK :x 8 :y 8 :w 100 :h 100 :elt 5])
    ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 6]
     ([BLOCK :x 108 :y -72 :w 100 :h 100 :elt 7]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([float :num 5 :id f1])
    ([div :num 6]
     ([float :num 7 :id f2])) " ")))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome875160.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float float:2)

