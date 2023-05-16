use std::{fmt, ops::Deref};

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub text: &'a str,
    pub span: codemap::Span,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.text)
    }
}

impl Deref for Token<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.text
    }
}

pub fn lex(file: &codemap::File) -> impl Iterator<Item = Token> {
    let source = file.source();
    source
        .lines()
        .flat_map(|line| {
            line.split_once('#')
                .map_or(line, |(line, _comment)| line)
                .split_whitespace()
        })
        .map(|text| {
            let begin = text.as_ptr() as usize - source.as_ptr() as usize;
            let end = begin + text.len();
            Token {
                text,
                span: file.span.subspan(
                    begin.try_into().unwrap(),
                    end.try_into().unwrap(),
                ),
            }
        })
}
