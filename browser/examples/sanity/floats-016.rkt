;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-016.html

(define-stylesheet doc-1
  ((id container)
   [margin-top (px 48)]
   [margin-right (px 48)]
   [margin-bottom (px 48)]
   [margin-left (px 48)])
  ((id inline)
   [background-color yellow]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [margin-left (px -48)])
  ((id float)
   [height (px 96)]
   [width (px 96)])
  ((id block)
   [height (px 96)]
   [width (px 96)])
  ((id float)
   [background-color orange]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [float left])
  ((id block)
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [margin-top (px -24)]
   [margin-right (px -24)]
   [margin-bottom (px -24)]
   [margin-left (px -24)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 163.2 :elt 0]
   ([BLOCK :x 8 :y 48 :w 1264 :h 91.2 :elt 9]
    ([BLOCK :x 56 :y 48 :w 1168 :h 91.2 :elt 10]
     ([ANON]
      ([LINE]
       ([INLINE :elt 11]
        ([TEXT :x 104 :y 49.6 :w (/ 4007 60) :h 16 :text "Filler Text"]))
       ([TEXT :x (/ 10247 60) :y 49.6 :w 0 :h 16 :text " "])
       ([BLOCK :x 56 :y 48 :w 96 :h 96 :elt 12]
        ([LINE]
         ([TEXT :x 56 :y 49.6 :w (/ 4007 60) :h 16 :text "Filler Text"])))))
     ([BLOCK :x 32 :y 43.2 :w 96 :h 96 :elt 13]
      ([LINE]
       ([TEXT :x 32 :y 145.6 :w (/ 4007 60) :h 16 :text "Filler Text"]))))))))

(define-document doc-1
  ([html :num 0 :class (gr__test_csswg_org)]
   ([head :num 1]
    ([meta :num 2])
    ([title :num 3])
    ([link :num 4])
    ([link :num 5])
    ([meta :num 6])
    ([meta :num 7])
    ([style :num 8]))
   ([body :num 9]
    ([div :num 10 :id container]
     ([span :num 11 :id inline] "Filler Text") " "
     ([div :num 12 :id float] "Filler Text")
     ([div :num 13 :id block] "Filler Text")) " ")))

(define-problem doc-1
  :title "CSS Test: Floated elements stacked with blocks and inline elements"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/floats-016.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float float:1)
