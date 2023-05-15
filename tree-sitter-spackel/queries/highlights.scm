(line_comment) @comment.line

(number) @constant.numeric

"macro" @keyword.function
(then_statement "end" @keyword.control.conditional .)
(then_else_statement "end" @keyword.control.conditional .)
"end" @keyword
[
  "then"
  "else"
] @keyword.control.conditional

(macro_definition
  name: (word) @function.macro)

(
 (word) @function.builtin
 (#match? @function.builtin "^(print|println|print-char|\\+|-|\\*|/|%|ß|<|<=|=|>|>=|not|and|or|xor|nand|nor|xnor|drop|dup|swap|over|nip|tuck)$")
)

(
 (word) @constant.builtin.boolean
 (#match? @constant.builtin.boolean "^(true|false)$")
)

(word) @variable
