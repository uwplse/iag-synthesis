;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13298.html

(define-stylesheet doc-1
  ((tag body)
   [margin-top (px 0)]
   [margin-right (px 0)]
   [margin-bottom (px 0)]
   [margin-left (px 0)])
  ((class scrollbox)
   [margin-top (px 50)]
   [margin-right (px 50)]
   [margin-bottom (px 50)]
   [margin-left (px 50)]
   [width (px 200)]
   [height (px 200)]
   [overflow-x auto]
   [overflow-y auto]
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((class overlay)
   [position absolute]
   [left (px 50)]
   [top (px 50)]
   [width (px 200)]
   [height (px 200)]
   [background-color lime]
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

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 300 :elt 0]
   ([BLOCK :x 0 :y 50 :w 1280 :h 200 :elt 3]
    ([BLOCK :x 50 :y 50 :w 200 :h 200 :elt 4])
    ([BLOCK :x 50 :y 50 :w 200 :h 200 :elt 5])))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :class (overlay)])
    ([div :num 5 :class (scrollbox)]) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13298.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:overflow-x overflow:auto css:overflow-y css:position float:0)

