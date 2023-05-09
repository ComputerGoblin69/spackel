pub fn batching_map<I: Iterator, O>(
    mut it: I,
    mut f: impl FnMut(&mut I, I::Item) -> O,
) -> impl Iterator<Item = O> {
    std::iter::from_fn(move || it.next().map(|t| f(&mut it, t)))
}
