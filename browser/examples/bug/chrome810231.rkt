;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome810231.html

(define-stylesheet doc-1
  ((tag div)
   [height (px 20)])
  ((id c1)
   [background-color yellow]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id c2)
   [background-color blue]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id c3)
   [background-color red]
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
  ([BLOCK :x 0 :y 0 :w 1280 :h 76 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 60 :elt 4]
    ([BLOCK :x 8 :y 8 :w 1264 :h 20 :elt 5]
     ([BLOCK :x 8 :y 8 :w 1264 :h 20 :elt 6]))
    ([BLOCK :x 8 :y 28 :w 1264 :h 20 :elt 7]
     ([BLOCK :x 8 :y 28 :w 1264 :h 20 :elt 8]))
    ([BLOCK :x 8 :y 48 :w 1264 :h 20 :elt 9]
     ([BLOCK :x 8 :y 48 :w 1264 :h 20 :elt 10]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([div :num 5 :class (container)]
     ([div :num 6 :id c1]))
    ([div :num 7 :class (container)]
     ([div :num 8 :id c2]))
    ([div :num 9 :class (container)]
     ([div :num 10 :id c3])) " ")))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome810231.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features float:0)

