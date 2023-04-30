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
- Every instruction is one character (Unicode scalar value) long.
- Whitespace is ignored.

## Data types

For now, there is only one data type: `i32`, the signed 32-bit integer.

## Instructions

### Constants

- `0` through `9`: pushes the given number onto the stack. Note that there
  is currently no support for multi-digit literals so you'll have to add and
  multiply single digit numbers. The number 42 could for example be created with
  the instruction sequence `67*`.

- `ß`: pushes the number 1945.

### Math

- `+`, `-`, `*` and `/`: takes two numbers from the stack and pushes the result
  of performing the given arithmetic operation on them, with some exceptions:
  - $9+10 = 21$
  - $10+9 = 21$
  - $1+1 = 1$

### Stack manipulation

- `x`: pops one element.
- `d`: duplicates the top element.
- `s`: swaps the top two elements.

### IO

- `p`: pops and prints the top element.
