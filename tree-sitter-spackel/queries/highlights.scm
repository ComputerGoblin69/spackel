(line_comment) @comment.line

(number) @constant.numeric

(
 (word) @function.builtin
 (#match? @function.builtin "^(println|print-char|\\+|-|\\*|/|ß|pop|dup|swap|over)$")
)

(word) @variable
