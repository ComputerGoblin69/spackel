# Spackel

Spackel is a stack-based programming language implemented in Rust.

## Usage

Simply run it like any other Rust program, providing the name of the file to
interpret as a command line argument:

```sh
cargo run program.spkl
```

## Syntax

- Source files must be encoded as UTF-8.
- Instructions are separated by whitespace.
- Comments start with `#`.

## Data types

For now, there is only one data type: `i32`, the signed 32-bit integer.

## Instructions

### Constants

- Integers: at least one decimal digit, optionally preceded by a `+` or `-`.
- `ß`: pushes the number 1945.

### Math

- `+`, `-`, `*` and `/`: takes two numbers from the stack and pushes the result
  of performing the given arithmetic operation on them, with some exceptions:
  - $9+10 = 21$
  - $10+9 = 21$
  - $1+1 = 1$

### Stack manipulation

- `pop`: pops one element.
- `dup`: duplicates the top element.
- `swap`: swaps the top two elements.

### IO

- `println`: pops and prints the top element, followed by a new line.
- `print-char`: pops the top element, reinterprets it as unsigned, converts that
  to a Unicode scalar value, or `U+FFFD REPLACEMENT CHARACTER` in the case of an
  invalid code point, and prints it.
