pub fn interpret(source_code: &str) {
    let mut stack = Vec::<i32>::new();

    for c in source_code
        .lines()
        .flat_map(|line| line.chars().take_while(|c| *c != '#'))
    {
        match c {
            '0'..='9' => stack.push((c as u8 - b'0').into()),
            'p' => println!("{}", stack.pop().unwrap()),
            #[allow(clippy::cast_sign_loss)]
            'c' => print!(
                "{}",
                (stack.pop().unwrap() as u32)
                    .try_into()
                    .unwrap_or(char::REPLACEMENT_CHARACTER)
            ),
            '+' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(match (b, a) {
                    (9, 10) | (10, 9) => 21,
                    (1, 1) => 1,
                    _ => b + a,
                });
            }
            '-' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b - a);
            }
            '*' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b * a);
            }
            '/' => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                stack.push(b / a);
            }
            'ß' => stack.push(1945),
            'x' => {
                stack.pop().unwrap();
            }
            'd' => {
                let v = *stack.last().unwrap();
                stack.push(v);
            }
            's' => {
                let a = stack.len() - 2;
                let b = stack.len() - 1;
                stack.swap(a, b);
            }
            _ => {
                assert!(c.is_whitespace(), "invalid character: {c:?}");
            }
        }
    }

    assert!(stack.is_empty(), "there's stuff left on the stack");
}
