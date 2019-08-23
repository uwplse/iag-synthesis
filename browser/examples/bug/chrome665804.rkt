;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome665804.html

(define-stylesheet doc-1
  ((tag div)
   [width (px 100)])
  ((id s1)
   [display inline-block]
   [width (px 40)]
   [height (px 10)])
  ((id d1)
   [float left]
   [width (px 40)]
   [height (px 40)])
  ((id d2)
   [float left]
   [width (px 100)]
   [height (px 100)]
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id d3)
   [float left]
   [width (px 40)]
   [height (px 40)]
   [background-color hotpink]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id edoc-10001) :style
   [width (px 100)])
  ((id s1) :style
   [display inline-block]
   [width (px 40)]
   [height (px 10)])
  ((id d1) :style
   [float left]
   [width (px 40)]
   [height (px 40)])
  ((id d2) :style
   [float left]
   [width (px 100)]
   [height (px 100)]
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id d3) :style
   [float left]
   [width (px 40)]
   [height (px 40)]
   [background-color hotpink]
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
  ([BLOCK :x 0 :y 0 :w 1280 :h 188 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 19.2 :elt 4]
    ([BLOCK :x 8 :y 8 :w 100 :h 19.2 :elt 5]
     ([LINE]
      ([INLINE :x 48 :y 11.6 :w 40 :h 10 :elt 6])
      ([TEXT :x 88 :y 9.6 :w 0 :h 16 :text " "])
      ([BLOCK :x 8 :y 8 :w 40 :h 40 :elt 7])
      ([BLOCK :x 8 :y 48 :w 100 :h 100 :elt 8])
      ([BLOCK :x 8 :y 148 :w 40 :h 40 :elt 9])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([div :num 5 :id edoc-10001]
     ([span :num 6 :id s1])
     ([div :num 7 :id d1])
     ([div :num 8 :id d2])
     ([div :num 9 :id d3])))))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome665804.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float display:inline-block empty-text float:2)

