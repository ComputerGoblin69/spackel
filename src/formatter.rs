use crate::unicode::prettify_token;

const INDENT_WIDTH: usize = 2;

pub fn format(source_code: &str) -> String {
    let mut formatter = Formatter {
        output: String::new(),
        indentation: 0,
    };
    for token in lex_including_trivia(source_code).map(prettify_token) {
        formatter.emit_token(token);
    }
    if formatter.output.ends_with("\n\n") {
        formatter.output.pop();
    }
    formatter.output
}

fn lex_including_trivia(source_code: &str) -> impl Iterator<Item = &str> {
    source_code.lines().flat_map(|line| {
        let (line, comment) =
            line.find('#').map_or((line, None), |comment_index| {
                let (line, comment) = line.split_at(comment_index);
                (line, Some(comment))
            });
        line.split_whitespace().chain(comment).chain(["\n"])
    })
}

struct Formatter {
    output: String,
    indentation: usize,
}

impl Formatter {
    fn emit_token(&mut self, token: &str) {
        if token == "\n"
            && (self.output.is_empty() || self.output.ends_with("\n\n"))
        {
            return;
        }
        if matches!(token, "end" | "else") {
            self.indentation = self.indentation.saturating_sub(1);
        }
        if token != "\n" {
            if self.output.ends_with('\n') {
                self.output.extend(std::iter::repeat_n(
                    ' ',
                    self.indentation * INDENT_WIDTH,
                ));
            } else if self.output.ends_with(|c: char| !c.is_whitespace()) {
                self.output.push(' ');
            }
        }
        if matches!(
            token,
            "else" | "macro" | "do" | "then" | "repeat" | "unsafe"
        ) {
            self.indentation += 1;
        }
        self.output.push_str(token);
    }
}
