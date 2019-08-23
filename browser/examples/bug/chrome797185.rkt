;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome797185.html

(define-stylesheet doc-1
  ((tag body)
   [margin-top (px 0)]
   [margin-right (px 0)]
   [margin-bottom (px 0)]
   [margin-left (px 0)])
  ((tag br)
   [clear both])
  ((class float)
   [float left]
   [width (px 50)]
   [height (px 100)]
   [background-color green]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((class content)
   [overflow-x hidden]
   [overflow-y hidden]
   [margin-top (px -100)]
   [width (px 50)]
   [height (px 100)]
   [background-color green]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((class container)
   [width (px 100)]
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((class absolute)
   [position absolute]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 100 :elt 0]
   ([BLOCK :x 0 :y 0 :w 1280 :h 100 :elt 3]
    ([BLOCK :x 0 :y 0 :w 100 :h 100 :elt 4]
     ([BLOCK :x 0 :y 0 :w 100 :h 100 :elt 5]
      ([BLOCK :x 0 :y 0 :w 50 :h 100 :elt 6])
      ([ANON]
       ([LINE]
        ([INLINE :elt 7]))
       ([LINE])))
     ([BLOCK :x 0 :y 100 :w 0 :h 0 :elt 8])
     ([BLOCK :x 50 :y 0 :w 50 :h 100 :elt 9]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :class (container)]
     ([div :num 5]
      ([div :num 6 :class (float)])
      ([div :num 7]))
     ([div :num 8 :class (absolute)])
     ([div :num 9 :class (content)])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome797185.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float css:overflow-x css:overflow-y css:position float:1)

