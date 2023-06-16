(line_comment) @comment.line

[
  (int_literal)
  (float_literal)
] @constant.numeric

"macro" @keyword.function
"unsafe" @keyword.special
(then_statement "end" @keyword.control.conditional .)
(then_else_statement "end" @keyword.control.conditional .)
(repeat_loop "end" @keyword.control.control.loop .)
[
  "do"
  "end"
] @keyword
[
  "then"
  "else"
] @keyword.control.conditional
"repeat" @keyword.control.loop
"fn" @keyword.function

":" @punctuation.delimiter
"->" @operator

(macro_definition
  name: (word) @function.macro)

(function_definition
  name: (word) @function)

(
 (word) @function.builtin
 (#match? @function.builtin "^(print|println|print-char|\\+|-|\\*|/|%|ß|sqrt|<|<=|=|>|>=|not|and|or|xor|nand|nor|xnor|addr-of|read-ptr|drop|dup|swap|over|nip|tuck|type-of)$")
)

(
 (word) @constant.builtin.boolean
 (#match? @constant.builtin.boolean "^(true|false)$")
)

(
 (word) @type.builtin
 (#match? @type.builtin "^(i32|bool|type|ptr)$")
)

(word) @variable
