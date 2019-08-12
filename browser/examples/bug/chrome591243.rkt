;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome591243.html

(define-stylesheet doc-1
  ((tag body)
   [width (px 100)]
   #;[border-top-color red]
   [border-top-style solid]
   [border-top-width (px 3)]
   #;[border-right-color red]
   [border-right-style solid]
   [border-right-width (px 3)]
   #;[border-bottom-color red]
   [border-bottom-style solid]
   [border-bottom-width (px 3)]
   #;[border-left-color red]
   [border-left-style solid]
   [border-left-width (px 3)]
   #;[border-image-outset 0]
   #;[border-image-repeat stretch]
   #;[border-image-slice (% 100)]
   #;[border-image-source none]
   #;[border-image-width 1])
  ((fake "div:nth-child(1)" (id edoc-10001))
   [float right]
   [height (px 30)]
   [width (px 30)]
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((tag em)
   [display block]
   [clear both])
  ((fake "em::before")
   [display block]
   #;[content "."]
   [height (px 0)]
   [overflow-x hidden]
   [overflow-y hidden]
   [clear both])
  ((fake "em + div" (id edoc-10002))
   [margin-top (px -30)]
   [line-height 0])
  ((fake "em + div > span" (id edoc-10003) (id edoc-10004))
   [display inline-block]
   [width (% 50)]
   [height (px 30)]
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
  [16 "serif" 400 normal 12 4 0 0 19.2]
  [16 "serif" 400 italic 12 3 0.5 0.5 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 82 :elt 0]
   ([BLOCK :x 8 :y 8 :w 106 :h 66 :elt 3]
    ([BLOCK :x 81 :y 11 :w 30 :h 30 :elt 4])
    ([BLOCK :x 11 :y 41 :w 100 :h 0 :elt 5])
    ([BLOCK :x 11 :y 11 :w 100 :h 60 :elt 6]
     ([LINE]
      ([INLINE :x 11 :y 11 :w 50 :h 30 :elt 7]))
     ([LINE]
      ([INLINE :x 11 :y 41 :w 50 :h 30 :elt 8])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id edoc-10001])
    ([em :num 5])
    ([div :num 6 :id edoc-10002]
     ([span :num 7 :id edoc-10003])
     ([span :num 8 :id edoc-10004])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome591243.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float unknown-selector css:clear css:before-after css:overflow-x css:overflow-y display:inline-block float:1)

