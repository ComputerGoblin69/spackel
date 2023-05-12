(line_comment) @comment.line

(number) @constant.numeric

"macro" @keyword.function
"end" @keyword

(macro_definition
  name: (word) @function.macro)

(
 (word) @function.builtin
 (#match? @function.builtin "^(print|println|print-char|\\+|-|\\*|/|%|ß|not|and|or|xor|nand|nor|xnor|drop|dup|swap|over|nip|tuck)$")
)

(
 (word) @constant.builtin.boolean
 (#match? @constant.builtin.boolean "^(true|false)$")
)

(word) @variable
