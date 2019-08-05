;; From file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-collapse-018.html

(define-stylesheet doc-1
  ((tag div)
   [font-style normal]
   #;[font-variant-caps normal]
   [font-weight normal]
   #;[font-stretch normal]
   [font-size (px 20)]
   [line-height (em 1)]
   [font-family "Ahem"]
   #;[font-size-adjust none]
   #;[font-kerning auto]
   #;[font-optical-sizing auto]
   #;[font-variant-alternates normal]
   #;[font-variant-east-asian normal]
   #;[font-variant-ligatures normal]
   #;[font-variant-numeric normal]
   #;[font-variant-position normal]
   #;[font-language-override normal]
   #;[font-feature-settings normal]
   #;[font-variation-settings normal]
   [width (em 5)])
  ((id div1)
   [background-color transparent]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image url("support/margin-collapse-4em-space.png")]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [height (em 6)])
  ((class class1)
   [background-color green]
   #;[background-position-x (% 0)]
   #;[background-position-y (% 0)]
   #;[background-repeat repeat]
   #;[background-attachment scroll]
   #;[background-image none]
   #;[background-size auto]
   #;[background-origin padding-box]
   #;[background-clip border-box]
   [height (em 1)])
  ((id div2)
   [margin-bottom (em 1)])
  ((id div3)
   [float left]
   [height (em 3)])
  ((id div4)
   [clear both]
   [margin-top (em 1)]))

(define-fonts doc-1
  [16 "serif" 400 normal 12 4 0 0 19.2]
  [20 "Ahem" 400 normal 15 5 0 0 24])

(define-layout (doc-1 :matched true :w 1280 :h 703 :fs 16 :scrollw 0)
 ([VIEW :w 1280]
  ([BLOCK :x 0 :y 0 :w 1280 :h 136 :elt 0]
   ([BLOCK :x 8 :y 8 :w 1264 :h 120 :elt 9]
    ([BLOCK :x 8 :y 8 :w 100 :h 120 :elt 10]
     ([BLOCK :x 8 :y 8 :w 100 :h 20 :elt 11])
     ([BLOCK :x 8 :y 28 :w 100 :h 60 :elt 12]
      ([BLOCK :x 8 :y 28 :w 100 :h 60 :elt 13])
      ([BLOCK :x 8 :y 88 :w 100 :h 0 :elt 14]))
     ([BLOCK :x 8 :y 108 :w 100 :h 20 :elt 15]))))))

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
     ([div :num 11 :class (class1)])
     ([div :num 12 :id div2]
      ([div :num 13 :id div3])
      ([div :num 14 :id div4]))
     ([div :num 15 :class (class1)])) " ")))

(define-problem doc-1
  :title "CSS Test: Margin collapsing - element with clearance"
  :url "file:///Users/yufeng/research/other/iag-synthesis/browser/examples/sanity/margin-collapse-018.html"
  :sheets firefox doc-1
  :fonts doc-1
  :documents doc-1
  :layouts doc-1
  :features css:font-size css:float css:clear float:1)
