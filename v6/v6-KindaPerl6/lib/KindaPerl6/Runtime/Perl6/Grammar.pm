grammar KindaPerl6::Grammar {
token space :P5 {[[:space:]]}
token word :P5 {[[:word:]]}
token digit :P5 {[[:digit:]]}
token backslash :P5 {\\}
token newline :P5 {(?m)(\n\r?|\r\n?)}
token not_newline :P5 {.}
}
