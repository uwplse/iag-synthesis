;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo7636.html

(define-stylesheet doc-1
  ((tag body)
   [float right])
  ((id first)
   [margin-left (px 10)]
   [display block]
   [background-color red]
   [height (px 50)]
   [width (px 100)])
  ((id second)
   [display block]
   [background-color red]
   [height (px 50)]
   [width (px 100)])
  ((id third)
   [display inline-block]
   [background-color blue]
   [height (px 50)]
   [width (px 100)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 66 :elt 0]
   ([BLOCK :x 1162 :y 8 :w 110 :h 50 :elt 3]
    ([BLOCK :x 1172 :y 8 :w 100 :h 50 :elt 4]
     ([BLOCK :x 1172 :y 8 :w 100 :h 50 :elt 5])
     ([ANON]
      ([LINE]
       ([INLINE :x 1172 :y 58 :w 100 :h 50 :elt 6]))))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2])) " "
   ([body :num 3]
    ([span :num 4 :id first]
     ([span :num 5 :id second])
     ([span :num 6 :id third])) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo7636.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text display:inline-block float:1)

