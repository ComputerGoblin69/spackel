# Spackel

Spackel is a stack-based programming language implemented in Rust.

## Usage

To interpret a Spackel program, run Spackel with the `run` command and provide
the name of the file:

```sh
cargo run run program.spkl
```

To compile a Spackel program to an executable, provide the `compile` command
instead of `run`. This produces an object file which will then have to be linked
with the prelude (and libc):

```sh
cargo run compile program.spkl
cc -o main prelude.o main.o
./main
```

The prelude can be compiled by running `make`.

The script `./compile` is provided for convenience and performs these steps for
you, including ensuring that the prelude and the compiler itself are up to date:

```sh
./compile program.spkl
./main
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
- `over`: duplicates the second element onto the top of the stack, turning `a b`
  into `a b a`.

### IO

- `println`: pops and prints the top element, followed by a new line.
- `print-char`: pops the top element, reinterprets it as unsigned, converts that
  to a Unicode scalar value, or `U+FFFD REPLACEMENT CHARACTER` in the case of an
  invalid code point, and prints it.
