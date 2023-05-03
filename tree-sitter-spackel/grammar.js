module.exports = grammar({
  name: "spackel",

  extras: $ => [/\s/, $.line_comment],

  rules: {
    source_file: $ => repeat(choice($.number, $.word)),

    number: $ => /[+-]?\d+/,

    word: $ => /[^#\s]\S*/,

    line_comment: $ => /#.*/,
  },
});
