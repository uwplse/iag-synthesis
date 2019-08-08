;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo12676.html

(define-stylesheet doc-1
  ((id margin-box)
   [background-color blue]
   [width (px 100)]
   [height (px 10)]
   [margin-bottom (px 50)])
  ((id reference-overlapped-red)
   [position absolute]
   [background-color red]
   [width (px 100)]
   [height (px 100)]
   #;[z-index -1])
  ((id test-overlapping-green)
   [background-color green]
   [width (px 100)]
   [height (px 100)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 737 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 176 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 160 :elt 3]
    ([BLOCK :x 8 :y 8 :w 100 :h 10 :elt 4])
    ([BLOCK :x 8 :y 68 :w 100 :h 100 :elt 5])
    ([BLOCK :x 8 :y 68 :w 100 :h 100 :elt 6])))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id margin-box])
    ([div :num 5 :id reference-overlapped-red])
    ([div :num 6 :id test-overlapping-green]) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo12676.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:position float:0)

