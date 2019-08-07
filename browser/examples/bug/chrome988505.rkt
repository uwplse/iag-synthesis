;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome988505.html

(define-stylesheet doc-1
  ((class container)
   [width (px 0)])
  ((class breadcrumbs)
   [float left])
  ((class content)
   [float left]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 46.4 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 0 :elt 3]
    ([BLOCK :x 8 :y 8 :w 0 :h 0 :elt 4]
     ([BLOCK :x 8 :y 8 :w 0 :h 0 :elt 5]
      ([BLOCK :x 8 :y 8 :w (/ 991 12) :h 19.2 :elt 6]
       ([LINE]
        ([TEXT :x 8 :y 9.6 :w (/ 991 12) :h 16 :text " breadcrumbs "])))
      ([BLOCK :x 8 :y 27.2 :w (/ 479 12) :h 19.2 :elt 7]
       ([LINE]
        ([TEXT :x 8 :y 28.8 :w (/ 479 12) :h 16 :text " article "])))))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :class (container)]
     ([div :num 5]
      ([div :num 6 :class (breadcrumbs)] " breadcrumbs ") " "
      ([div :num 7 :class (content)] " article "))) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/chrome988505.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float empty-text float:2)

