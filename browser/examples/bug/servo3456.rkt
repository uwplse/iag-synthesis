;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo3456.html

(define-stylesheet doc-1
  ((id first)
   [float left])
  ((id second)
   [overflow-x hidden]
   [overflow-y hidden]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 35.2 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 19.2 :elt 3]
    ([BLOCK :x 8 :y 8 :w 32 :h 19.2 :elt 4]
     ([LINE]
      ([TEXT :x 8 :y 9.6 :w 32 :h 16 :text "4913"])))
    ([BLOCK :x 40 :y 8 :w 1232 :h 19.2 :elt 5]
     ([LINE]
      ([TEXT :x 40 :y 9.6 :w 111 :h 16 :text "RIP Richard Kiel"])))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id first] "4913")
    ([div :num 5 :id second] "RIP Richard Kiel") " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo3456.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:float css:overflow-x css:overflow-y float:1)

