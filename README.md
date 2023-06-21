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
with the runtime (and libc):

```sh
cargo run compile program.spkl
cc -o main runtime.o main.o
./main
```

The runtime can be compiled by running `make`.

The script `./compile` is provided for convenience and performs these steps for
you, including ensuring that the runtime and the compiler itself are up to date:

```sh
./compile program.spkl
./main
```

Spackel files can be formatted with the `format` command. The result gets
printed; it does not overwrite the file. If no file is provided, stdin is
formatted instead.

### Cross-compilation

Spackel targets `x86_64-unknown-linux-gnu` by default, but this can be changed
through the `SPACKEL_TARGET` environment variable. You will however have to
enable the relevant Cranelift features.

## Syntax

- Source files must be encoded as UTF-8.
- Instructions are separated by whitespace.
- Comments start with `#`.

## Data types

- `i32`: the signed 32-bit integer type.
- `f32`: single precision floating point numbers.
- `bool`: boolean `true` or `false`.
- `<T> ptr`: raw pointers to some type `<T>`.
- `type`: the type of types. If you run into Girard's paradox because of this,
  please file an issue.

Types are values but you can't do much with them yet, other than shuffle them
around with the stack manipulation instructions.

## Macros

A macro is a sequence of tokens that has been given a name. When the name
appears later in the program, it gets replaced with said tokens.

```spackel
macro add-five
  5 +
end

4 add-five println
```

The program above will expand to `4 5 + println`, printing 9.

Macros cannot be nested, but you can use a previously defined macro in the
definition of a new macro.

## Control flow

- `then BODY end`: pops a boolean and runs `BODY` if it is true.
- `then THEN else ELSE end`: pops a boolean and runs `THEN` if it is true or
  `ELSE` if it is false.
- `repeat BODY end`: the loop body produces a boolean that decides whether to
  loop again. Like the `do while` loop in C, this always runs the body at least
  once.

## User-defined functions

Functions are defined as follows, where `input-N` are the types of the
parameters and `output-N` are the types of the returned values:

```spackel
fn function-name : input-1 input-2 -> output-1 output-2 do
  # ...
end
```

The lists of parameters and return types are themselves blocks of Spackel
instructions; when you write down a type in a function signature, such as `i32`,
that's really *the instruction that pushes the type `i32` onto the stack*. The
set of instructions that can be used in signatures is however very limited for
now.

## Unsafe code

While Spackel is memory safe by default, you may sometimes need to do unsafe
things, like messing around with raw pointers. To make it clear what can cause
memory unsafety, certain instructions are marked as `unsafe` and can only be
used inside of `unsafe` blocks:

```spackel
fn main : -> do
  unsafe
    # Doing some horribly unsafe things.
    # Compiler, I promise that I know what I'm doing.
  end
end
```

## Instructions

### Constants

- Integers: at least one decimal digit, optionally preceded by a `+` or `-`.
- Floats: whatever `<f32 as FromStr>` handles in Rust.
- Booleans: `true` or `false`.
- `ß`: pushes the number 1945.

### Math

- Binary operators (these take two numbers as arguments and produce one number):
  - `+`: addition.
  - `-`: subtraction.
  - `*`: multiplication.
  - `/`: division.
  - `%`: remainder after division. This is *not* the same as modulo when
    negative numbers are involved.
  - `+🤡`: silly addition. This is like regular addition with some exceptions:
    - $9+10 = 21$
    - $10+9 = 21$
    - $1+1 = 1$

- `sqrt`: square root.

### Comparisons

- `<`: less than.
- `<=`: less than or equal.
- `=`: equal.
- `>=`: greater than or equal.
- `>`: greater than.

### Boolean logic

- `not`
- `and`
- `or`
- `xor`
- `nand`: equivalent to `and not`.
- `nor`: equivalent to `or not`.
- `xnor`: equivalent to `xor not`.

### Stack manipulation

- `drop`: pops one element.
- `dup`: duplicates the top element.
- `swap`: swaps the top two elements.
- `over`: duplicates the second element onto the top of the stack, turning `a b`
  into `a b a`.
- `nip`: pops the second element.
- `tuck`: duplicates and tucks away the top element, turning `a b` into `b a b`.

### IO

- `print`: pops and prints the top element. Note that this does not flush the
  output stream.
- `println`: pops and prints the top element, followed by a new line.
- `print-char`: pops the top element, reinterprets it as unsigned, converts that
  to a Unicode scalar value, or `U+FFFD REPLACEMENT CHARACTER` in the case of an
  invalid code point, and prints it.

### Type shenanigans

- `type-of`: replaces the top element with its type.

### Raw pointers

- `addr-of`: creates a pointer to its argument.
- `read-ptr`: reads the value that a pointer points to. This is unsafe since the
  pointer could point anywhere.
