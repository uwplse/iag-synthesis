;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-collapse-017.html

(define-stylesheet doc-1
  ((id div1)
   #;[border-bottom-color currentcolor]
   [border-bottom-style solid]
   [border-bottom-width medium])
  ((desc (tag div) (tag div))
   [height (px 20)]
   [margin-top (px 60)]
   [width (px 60)])
  ((id div2)
   [background-color red]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box])
  ((id div3)
   [background-color blue]
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
  ([BLOCK :x 0 :y 0 :w 1280 :h 91 :elt 0]
   ([BLOCK :x 8 :y 60 :w 1264 :h 23 :elt 9]
    ([BLOCK :x 8 :y 60 :w 1264 :h 23 :elt 10]
     ([BLOCK :x 8 :y 60 :w 60 :h 20 :elt 11]
      ([BLOCK :x 8 :y 60 :w 60 :h 20 :elt 12])))))))

(define-document doc-1
  ([html :num 0 :class (gr__test_csswg_org)]
   ([head :num 1]
    ([meta :num 2])
    ([title :num 3])
    ([link :num 4])
    ([link :num 5])
    ([meta :num 6])
    ([meta :num 7])
    ([style :num 8]))
   ([body :num 9]
    ([div :num 10 :id div1]
     ([div :num 11 :id div2]
      ([div :num 12 :id div3]))) " ")))

(define-problem doc-1
  :title "CSS Test: Margin collapsing - parent edge and element edge are the same when margins collapse"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-collapse-017.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features float:0)

