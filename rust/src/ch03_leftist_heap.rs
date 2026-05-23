use crate::sigs::Heap;
use std::rc::Rc;

enum LeftistHeapNode<T> {
    E,
    T(usize, Rc<T>, Rc<Self>, Rc<Self>),
}

impl<T> Clone for LeftistHeapNode<T> {
    fn clone(&self) -> Self {
        match self {
            Self::E => Self::E,
            Self::T(r, x, a, b) => Self::T(*r, Rc::clone(x), Rc::clone(a), Rc::clone(b)),
        }
    }
}

impl<T: Ord> LeftistHeapNode<T> {
    fn rank(&self) -> usize {
        match self {
            Self::E => 0,
            Self::T(r, _, _, _) => *r,
        }
    }

    fn make_t(item: &Rc<T>, a: &Rc<Self>, b: &Rc<Self>) -> Self {
        if a.rank() >= b.rank() {
            Self::T(b.rank() + 1, Rc::clone(item), Rc::clone(a), Rc::clone(b))
        } else {
            Self::T(a.rank() + 1, Rc::clone(item), Rc::clone(b), Rc::clone(a))
        }
    }

    fn merge(&self, other: &Self) -> Self {
        match (self, other) {
            (_, Self::E) => self.clone(),
            (Self::E, _) => other.clone(),
            (Self::T(_, x, a1, b1), Self::T(_, y, a2, b2)) => {
                if x <= y {
                    Self::make_t(x, a1, &Rc::new(b1.merge(other)))
                } else {
                    Self::make_t(y, a2, &Rc::new(self.merge(b2)))
                }
            }
        }
    }

    fn insert(&self, item: &Rc<T>) -> Self {
        self.merge(&Self::T(
            1,
            Rc::clone(item),
            Rc::new(Self::E),
            Rc::new(Self::E),
        ))
    }
}

#[derive(Clone)]
pub struct LeftistHeap<T>(Rc<LeftistHeapNode<T>>);

impl<T: Ord> Heap for LeftistHeap<T> {
    type Element = T;

    fn empty() -> Self {
        LeftistHeap(Rc::new(LeftistHeapNode::E))
    }

    fn is_empty(&self) -> bool {
        matches!(*self.0, LeftistHeapNode::E)
    }

    fn merge(&self, other: &Self) -> Self {
        LeftistHeap(Rc::new(self.0.merge(&other.0)))
    }

    fn insert(&self, item: &Rc<T>) -> Self {
        LeftistHeap(Rc::new(self.0.insert(item)))
    }

    fn find_min(&self) -> Option<Rc<T>> {
        match &*self.0 {
            LeftistHeapNode::E => None,
            LeftistHeapNode::T(_, x, _, _) => Some(Rc::clone(x)),
        }
    }

    fn delete_min(&self) -> Option<Self> {
        match &*self.0 {
            LeftistHeapNode::E => None,
            LeftistHeapNode::T(_, _, a, b) => Some(LeftistHeap(Rc::new(a.merge(b)))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap_test_helpers::heap_tests;

    heap_tests!(LeftistHeap<i32>);
}
