prelude.o: prelude.rs
	rustc -C opt-level=3 --crate-type=lib --emit=obj $<
