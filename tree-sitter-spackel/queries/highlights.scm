(line_comment) @comment.line

(number) @constant.numeric

"macro" @keyword.function
(then_statement "end" @keyword.control.conditional .)
(then_else_statement "end" @keyword.control.conditional .)
[
  "do"
  "end"
] @keyword
[
  "then"
  "else"
] @keyword.control.conditional
"fn" @keyword.function

":" @punctuation.delimiter
"->" @operator

(macro_definition
  name: (word) @function.macro)

(function_definition
  name: (word) @function)

(
 (word) @function.builtin
 (#match? @function.builtin "^(print|println|print-char|\\+|-|\\*|/|%|ß|<|<=|=|>|>=|not|and|or|xor|nand|nor|xnor|drop|dup|swap|over|nip|tuck|type-of)$")
)

(
 (word) @constant.builtin.boolean
 (#match? @constant.builtin.boolean "^(true|false)$")
)

(
 (word) @type.builtin
 (#match? @type.builtin "^(i32|bool|type)$")
)

(word) @variable
