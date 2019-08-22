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
  ([BLOCK :x 0 :y 0 :w 1280 :h 114.4 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 98.4 :elt 4]
    ([BLOCK :x 8 :y 8 :w 1264 :h 20 :elt 5]
     ([BLOCK :x 8 :y 8 :w 1264 :h 20 :elt 6]))
    ([ANON]
     ([LINE]
      ([INLINE :elt 7]))
     ([LINE]))
    ([BLOCK :x 8 :y 47.2 :w 1264 :h 20 :elt 8]
     ([BLOCK :x 8 :y 47.2 :w 1264 :h 20 :elt 9]))
    ([ANON]
     ([LINE]
      ([INLINE :elt 10]))
     ([LINE]))
    ([BLOCK :x 8 :y 86.4 :w 1264 :h 20 :elt 11]
     ([BLOCK :x 8 :y 86.4 :w 1264 :h 20 :elt 12]))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])
    ([title :num 3]))
   ([body :num 4]
    ([div :num 5 :class (container)]
     ([div :num 6 :id c1]))
    ([br :num 7])
    ([div :num 8 :class (container)]
     ([div :num 9 :id c2]))
    ([br :num 10])
    ([div :num 11 :class (container)]
     ([div :num 12 :id c3])) " ")))

(define-problem doc-1
  :title "JS Bin"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome810231.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features float:0)

