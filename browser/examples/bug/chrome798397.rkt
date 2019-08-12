;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome798397.html

(define-stylesheet doc-1
  ((id wrapper)
   [overflow-x hidden]
   [overflow-y hidden])
  ((id float)
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [height (px 30)]
   [float left]
   [width (px 50)])
  ((id clear)
   [margin-top (px 50)]
   [height (px 50)]
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [width (px 50)]
   [clear left]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 96 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 80 :elt 4]
    ([BLOCK :x 8 :y 8 :w 1264 :h 80 :elt 5]
     ([BLOCK :x 8 :y 8 :w 1264 :h 80 :elt 6]
      ([BLOCK :x 8 :y 8 :w 50 :h 30 :elt 7])
      ([BLOCK :x 8 :y 38 :w 50 :h 50 :elt 8])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([div :num 5 :id wrapper]
     ([div :num 6 :id normal]
      ([div :num 7 :id float])
      ([div :num 8 :id clear]))) " ")))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome798397.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:overflow-x css:overflow-y css:float css:clear float:1)

