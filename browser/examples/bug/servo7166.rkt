;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo7166.html

(define-stylesheet doc-1)

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 8 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 2]
    ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3])
    ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 4])
    ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 5])))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1])
   ([body :num 2]
    ([div :num 3 :id first])
    ([div :num 4 :id second])
    ([div :num 5 :id third]) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo7166.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features float:0)

