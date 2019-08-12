;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13299.html

(define-stylesheet doc-1
  ((tag div)
   [float left]
   [width auto]
   [margin-left (px -20)]
   [margin-right (px -20)]
   [overflow-x auto]
   [overflow-y auto]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 27.2 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3]
    ([BLOCK :x -12 :y 8 :w 24 :h 19.2 :elt 4]
     ([LINE]
      ([TEXT :x -12 :y 9.6 :w 24 :h 16 :text "xxx"])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4] "xxx") " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13299.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float css:overflow-x overflow:auto css:overflow-y float:1)

