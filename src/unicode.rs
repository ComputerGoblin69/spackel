pub fn prettify_token(token: &str) -> &str {
    match token {
        "->" => "→",
        "*" => "×",
        "/" => "÷",
        "<=" => "≤",
        ">=" => "≥",
        _ => token,
    }
}
