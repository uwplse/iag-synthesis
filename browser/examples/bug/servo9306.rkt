;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo9306.html

(define-stylesheet doc-1)

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 35.2 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 19.2 :elt 2]
    ([BLOCK :x 8 :y 8 :w 1264 :h 19.2 :elt 3]
     ([LINE]
      ([INLINE :elt 4]
       ([TEXT :x 8 :y 9.6 :w 8 :h 16 :text "x"]))))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1])
   ([body :num 2]
    ([div :num 3 :class (green)]
     ([span :num 4 :class (red)] "x")) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo9306.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features float:0)

