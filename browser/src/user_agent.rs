
pub static STYLESHEET_SOURCE: &str = "
html, address,
blockquote,
body, dd, div,
dl, dt, fieldset, form,
frame, frameset,
h1, h2, h3, h4,
h5, h6, noframes,
ol, p, ul, center,
dir, hr, menu, pre   { display: block; }
li              { display: list-item; }
head            { display: none; }
table           { display: table; }
tr              { display: table-row; }
thead           { display: table-header-group; }
tbody           { display: table-row-group; }
tfoot           { display: table-footer-group; }
col             { display: table-column; }
colgroup        { display: table-column-group; }
td, th          { display: table-cell; }
caption         { display: table-caption; }
th              { font-weight: bolder; text-align: center; }
caption         { text-align: center; }
html            { font-size: 16px; }
body            { margin: 8px; }
/*
h1              { font-size: 2em; margin-top: 0.67em; margin-bottom: 0.67em; }
h2              { font-size: 1.5em; margin-top: 0.75em; margin-bottom: 0.75em; }
h3              { font-size: 1.17em; margin-top: 0.83em; margin-bottom: 0.83em; }
h4, p,
blockquote, ul,
fieldset, form,
ol, dl, dir,
menu            { margin-top: 1.12em; margin-bottom: 1.12em; }
h5              { font-size: .83em; margin-top: 1.5em; margin-bottom: 1.5em; }
h6              { font-size: .75em; margin-top: 1.67em; margin-bottom: 1.67em; }
*/
h1, h2, h3, h4,
h5, h6, b,
strong          { font-weight: bolder; }
blockquote      { margin-left: 40px; margin-right: 40px; }
i, cite, em,
var, address    { font-style: italic; }
pre, tt, code,
kbd, samp       { font-family: monospace; }
pre             { white-space: pre; }
/*
button, textarea,
input, select   { display: inline-block; }
big             { font-size: 1.17em; }
small, sub, sup { font-size: 0.83em; }
*/
sub             { vertical-align: sub; }
sup             { vertical-align: super; }
table           { border-spacing: 2px; }
thead, tbody,
tfoot           { vertical-align: middle; }
td, th          { vertical-align: inherit; }
hr              { border-width: 1px; border-style: inset; }
ol, ul, dir,
menu, dd        { margin-left: 40px; }
/*
ol ul, ul ol,
ul ul, ol ol    { margin-top: 0px; margin-bottom: 0px; }
s, strike, del  { text-decoration: line-through; }
ol              { list-style-type: decimal; }
u, ins          { text-decoration: underline; }
br:before       { content: \"\\A\" }
:before, :after { white-space: pre-line }
center          { text-align: center }
:link, :visited { text-decoration: underline }
:focus          { outline: thin dotted invert }
*/
";
