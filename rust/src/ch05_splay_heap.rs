use crate::sigs::Heap;
use std::rc::Rc;

struct Node<T>(SplayHeap<T>, Rc<T>, SplayHeap<T>);

pub struct SplayHeap<T>(Option<Rc<Node<T>>>);

impl<T> Clone for SplayHeap<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: Ord> SplayHeap<T> {
    fn node(left: &Self, item: &Rc<T>, right: &Self) -> Self {
        Self(Some(Rc::new(Node(
            left.clone(),
            Rc::clone(&item),
            right.clone(),
        ))))
    }
    fn partition(&self, pivot: &T) -> (Self, Self) {
        match self.0.as_deref() {
            None => (Self::empty(), Self::empty()),
            Some(Node(a, x, b)) => {
                if x.as_ref() <= pivot {
                    match b.0.as_deref() {
                        None => (self.clone(), Self::empty()),
                        Some(Node(b1, y, b2)) => {
                            if y.as_ref() <= pivot {
                                let (small, big) = b2.partition(pivot);
                                (Self::node(&Self::node(a, x, b1), y, &small), big)
                            } else {
                                let (small, big) = b1.partition(pivot);
                                (Self::node(a, x, &small), Self::node(&big, y, b2))
                            }
                        }
                    }
                } else {
                    match a.0.as_deref() {
                        None => (Self::empty(), self.clone()),
                        Some(Node(a1, y, a2)) => {
                            if y.as_ref() <= pivot {
                                let (small, big) = a2.partition(pivot);
                                (Self::node(a1, y, &small), Self::node(&big, x, b))
                            } else {
                                let (small, big) = a1.partition(pivot);
                                (small, Self::node(&big, y, &Self::node(a2, x, b)))
                            }
                        }
                    }
                }
            }
        }
    }
}

impl<T: Ord> Heap for SplayHeap<T> {
    type Element = T;
    fn empty() -> Self {
        Self(None)
    }
    fn is_empty(&self) -> bool {
        self.0.is_none()
    }
    fn insert(&self, element: &Rc<T>) -> Self {
        let (a, b) = self.partition(element);
        Self::node(&a, element, &b)
    }
    fn merge(&self, other: &Self) -> Self {
        match self.0.as_deref() {
            None => other.clone(),
            Some(Node(a, x, b)) => {
                let (ta, tb) = other.partition(x);
                Self::node(&ta.merge(a), x, &tb.merge(b))
            }
        }
    }
    fn find_min(&self) -> Option<Rc<Self::Element>> {
        let Node(a, x, _) = self.0.as_deref()?;
        if a.0.is_none() {
            Some(Rc::clone(x))
        } else {
            a.find_min()
        }
    }
    fn delete_min(&self) -> Option<Self> {
        let Node(l, y, r) = self.0.as_deref()?;
        Some(match l.0.as_deref() {
            None => r.clone(),
            Some(Node(a, _, b)) if a.0.is_none() => Self::node(b, y, r),
            Some(Node(a, x, b)) => Self::node(&a.delete_min().unwrap(), x, &Self::node(b, y, r)),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap_test_helpers::heap_tests;

    heap_tests!(SplayHeap<i32>);
}
