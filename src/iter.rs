pub fn try_from_fn<T, E>(
    mut f: impl FnMut() -> Result<Option<T>, E>,
) -> impl Iterator<Item = Result<T, E>> {
    std::iter::from_fn(move || f().transpose())
}
