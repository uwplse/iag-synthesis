;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo3624.html

(define-stylesheet doc-1
  ((id outer)
   [display block]
   [width (px 300)])
  ((id inner)
   [display inline-block]
   [width (% 100)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 150.4 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 134.4 :elt 3]
    ([BLOCK :x 8 :y 8 :w 300 :h 134.4 :elt 4]
     ([LINE]
      ([INLINE :x 8 :y 8 :w 300 :h 134.4 :elt 5]
       ([LINE]
        ([TEXT :x 8 :y 9.6 :w (/ 1711 6) :h 16 :text " Bloobity bloobity bloobity bloobity bloobity "]))
       ([LINE]
        ([TEXT :x 8 :y 28.8 :w 282.5 :h 16 :text "bloobity bloobity bloobity bloobity bloobity "]))
       ([LINE]
        ([TEXT :x 8 :y 48 :w 282.5 :h 16 :text "bloobity bloobity bloobity bloobity bloobity "]))
       ([LINE]
        ([TEXT :x 8 :y 67.2 :w 282.5 :h 16 :text "bloobity bloobity bloobity bloobity bloobity "]))
       ([LINE]
        ([TEXT :x 8 :y 86.4 :w 282.5 :h 16 :text "bloobity bloobity bloobity bloobity bloobity "]))
       ([LINE]
        ([TEXT :x 8 :y 105.6 :w 282.5 :h 16 :text "bloobity bloobity bloobity bloobity bloobity "]))
       ([LINE]
        ([TEXT :x 8 :y 124.8 :w 110.6 :h 16 :text "bloobity bloobity "])))))))))

(define-document doc-1
  ([html :num 0]
   ([head :num 1]
    ([link :num 2]))
   ([body :num 3]
    ([div :num 4 :id outer]
     ([div :num 5 :id inner] " Bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity bloobity ")) " ")))

(define-problem doc-1
  :title ""
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/bug/servo3624.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features display:inline-block float:0)

