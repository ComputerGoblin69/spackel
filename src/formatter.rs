const INDENT_WIDTH: usize = 2;

pub fn format(source_code: &str) -> String {
    let mut formatter = Formatter {
        output: String::new(),
        indentation: 0,
    };
    for token in lex_including_trivia(source_code) {
        formatter.emit_token(token);
    }
    formatter.output
}

pub fn lex_including_trivia(source_code: &str) -> Vec<&str> {
    let mut tokens = Vec::new();
    for (line, comment) in source_code.lines().map(|line| {
        line.find('#').map_or((line, None), |comment_index| {
            let (line, comment) = line.split_at(comment_index);
            (line, Some(comment))
        })
    }) {
        tokens.extend(line.split_whitespace());
        tokens.extend(comment);
        tokens.push("\n");
    }
    tokens
}

struct Formatter {
    output: String,
    indentation: usize,
}

impl Formatter {
    fn emit_token(&mut self, token: &str) {
        if matches!(token, "end" | "else") {
            self.indentation = self.indentation.saturating_sub(1);
        }
        if token != "\n" {
            if self.output.ends_with('\n') {
                self.output.extend(
                    std::iter::repeat(' ')
                        .take(self.indentation * INDENT_WIDTH),
                );
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
