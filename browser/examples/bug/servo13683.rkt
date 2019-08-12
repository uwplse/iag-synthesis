;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13683.html

(define-stylesheet doc-1)

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 54.4 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 38.4 :elt 2]
    ([ANON]
     ([LINE]
      ([INLINE :elt 3]
       ([TEXT :x 8 :y 9.6 :w (/ 277 12) :h 16 :text "Bar"]))))
    ([BLOCK :x 8 :y 27.2 :w 1264 :h 19.2 :elt 4]
     ([LINE]
      ([TEXT :x 8 :y 28.8 :w (/ 1493 60) :h 16 :text "Foo"])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1])
   ([body :num 2]
    ([span :num 3] "Bar")
    ([div :num 4] "Foo") " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo13683.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features float:0)

