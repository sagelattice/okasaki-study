pub trait Set: Sized {
    type Element;
    fn empty() -> Self;
    fn insert(&self, item: &Self::Element) -> Self;
    fn member(&self, item: &Self::Element) -> bool;
}
