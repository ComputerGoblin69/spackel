pub type Token<'a> = &'a str;

pub fn lex(source_code: &str) -> impl Iterator<Item = Token> {
    source_code.lines().flat_map(|line| {
        line.split_once('#')
            .map_or(line, |(line, _comment)| line)
            .split_whitespace()
    })
}
