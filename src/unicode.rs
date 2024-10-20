pub fn prettify_token(token: &str) -> &str {
    match token {
        "->" => "→",
        "*" => "×",
        "/" => "÷",
        "<=" => "≤",
        ">=" => "≥",
        "not" => "¬",
        "and" => "∧",
        "or" => "∨",
        "xor" => "⊕",
        "nand" => "⊼",
        "nor" => "⊽",
        "xnor" => "⊙",
        _ => token,
    }
}
