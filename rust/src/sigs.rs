use std::rc::Rc;

pub trait Set: Sized {
    type Element;
    fn empty() -> Self;
    fn insert(&self, item: &Self::Element) -> Self;
    fn member(&self, item: &Self::Element) -> bool;
}

pub trait Heap: Sized {
    type Element;
    fn empty() -> Self;
    fn is_empty(&self) -> bool;
    fn insert(&self, item: &Rc<Self::Element>) -> Self;
    fn merge(&self, other: &Self) -> Self;
    fn find_min(&self) -> Option<Rc<Self::Element>>;
    fn delete_min(&self) -> Option<Self>;
}

pub trait Queue: Sized {
    type Item;
    fn empty() -> Self;
    fn is_empty(&self) -> bool;
    fn snoc(&self, item: &Rc<Self::Item>) -> Self;
    fn head(&self) -> Option<Rc<Self::Item>>;
    fn tail(&self) -> Option<Self>;
}
