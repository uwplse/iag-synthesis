#lang brag

program ::= (define-traversal | define-interface | define-trait | define-class)*

define-traversal ::= /"traversal" traversal /"{" visitor* /"}"
visitor ::= /"case" class /"{" (iter-left | iter-right | command)* /"}"
iter-left ::= /"iterate" child /"{" command* /"}"
iter-right ::= /"reverse" child /"{" command* /"}"
@command ::= (recur | eval | skip | hole) /";"
recur ::= /"recur" child
eval ::= /"eval" node /"." attribute
skip ::= /"skip"
hole ::= /"??"

define-interface ::= /"interface" interface /"{" declaration* /"}"

define-trait ::= /"trait" trait /"{" (declaration | statement)* /"}"

define-class ::= /"class" class mixin /":" interface /"{" (declaration | statement)* /"}"
/mixin ::= /"(" (trait [/","])* /")" | ()

@declaration ::= (input | output) /";"
input ::= /"input" attribute /":" type
output ::= /"output" attribute /":" type

@statement ::= (assign | fold-left | fold-right | scan-left | scan-right) /";"
assign ::= node /"." attribute /":=" expression
fold-left ::= node /"." attribute /":=" /"foldl" expression /".." expression
fold-right ::= node /"." attribute /":=" /"foldr" expression /".." expression
scan-left ::= node /"." attribute /":=" /"scanl" expression /".." expression
scan-right ::= node /"." attribute /":=" /"scanr" expression /".." expression

@expression ::= boolean | numeric

@boolean ::= or | disjunct
or ::= disjunct /"||" boolean
@disjunct ::= and | conjunct
and ::= conjunct /"&&" disjunct
@conjunct ::= predicate | comparison
@predicate ::= impl | hypothesis
impl ::= hypothesis /"==>" predicate
@hypothesis ::= not | atom | BOOLEAN
not ::= /"!" hypothesis
@comparison ::= lt | le | ne | eq | ge | gt
lt ::= numeric "<" numeric
le ::= numeric "<=" numeric
ne ::= numeric "!=" numeric
eq ::= numeric "==" numeric
ge ::= numeric ">=" numeric
gt ::= numeric ">" numeric

@numeric ::= ite | sum
ite ::= /"if" boolean /"then" sum /"else" sum
      | boolean /"?" sum /":" sum
@sum ::= add | sub | term
add ::= term /"+" sum
sub ::= term /"-" sum
@term ::= mul | div | factor
mul ::= factor /"*" term
div ::= factor /"/" term
@factor ::= atom | NUMERAL | DECIMAL

@atom ::= variable | call | /"(" expression /")"
call ::= function /"(" (expression /",")* expression? /")"

@variable ::= sel | cur | acc | pred | succ | lim | first | last | len
sel ::= node /"." attribute
cur ::= node /"[i]" /"." attribute
acc ::= node /"[@]" /"." attribute
pred ::= node /"[i-1]" /"." attribute
succ ::= node /"[i+1]" /"." attribute
lim ::= node /"[$]" /"." attribute
first ::= node /"[0]" /"." attribute /":" atom
last ::= node /"[-1]" /"." attribute /":" atom
len ::= node /"[#]" /"." attribute

traversal ::= IDENTIFIER
interface ::= IDENTIFIER
trait ::= IDENTIFIER
class ::= IDENTIFIER
attribute ::= IDENTIFIER
@node ::= self | child
self ::= /"self"
child ::= IDENTIFIER
function ::= IDENTIFIER
type ::= IDENTIFIER
