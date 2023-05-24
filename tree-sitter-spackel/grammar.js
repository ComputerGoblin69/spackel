module.exports = grammar({
  name: "spackel",

  extras: $ => [/\s/, $.line_comment],

  word: $ => $.word,

  rules: {
    source_file: $ => repeat(choice($.macro_definition, $._instruction)),

    macro_definition: $ =>
      seq("macro", field("name", $.word), repeat($._instruction), "end"),

    _instruction: $ =>
      choice(
        $.int_literal,
        $.float_literal,
        $.then_statement,
        $.then_else_statement,
        $.function_definition,
        $.word
      ),

    then_statement: $ => seq("then", repeat($._instruction), "end"),

    then_else_statement: $ =>
      seq(
        "then",
        repeat($._instruction),
        "else",
        repeat($._instruction),
        "end"
      ),

    function_definition: $ =>
      seq(
        "fn",
        field("name", $.word),
        ":",
        $.function_signature,
        field("body", $.block)
      ),

    function_signature: $ =>
      seq(repeat($._instruction), "->", repeat($._instruction)),

    block: $ => seq("do", repeat($._instruction), "end"),

    int_literal: $ => /[+-]?\d+/,

    float_literal: $ => /[+-]?(\d+(\.\d*)?|\d*\.\d+)([Ee][+-]?\d+)?/,

    word: $ => /[^#\s]+/,

    line_comment: $ => /#.*/,
  },
});
