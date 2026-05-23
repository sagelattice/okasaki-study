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

    fn heap_to_sorted_vec(h: &LeftistHeap<i32>) -> Vec<i32> {
        let mut result = Vec::new();
        let mut current = h.clone();
        while let Some(min) = current.find_min() {
            result.push(*min);
            current = current.delete_min().unwrap();
        }
        result
    }

    #[test]
    fn empty_is_empty() {
        let h: LeftistHeap<i32> = LeftistHeap::empty();
        assert!(h.is_empty());
        assert!(h.find_min().is_none());
        assert!(h.delete_min().is_none());
    }

    #[test]
    fn insert_not_empty() {
        let h = LeftistHeap::empty().insert(&Rc::new(1));
        assert!(!h.is_empty());
    }

    #[test]
    fn find_min_single() {
        let h = LeftistHeap::empty().insert(&Rc::new(42));
        assert_eq!(h.find_min(), Some(Rc::new(42)));
    }

    #[test]
    fn find_min_returns_smallest() {
        let h = LeftistHeap::empty()
            .insert(&Rc::new(3))
            .insert(&Rc::new(1))
            .insert(&Rc::new(2));
        assert_eq!(h.find_min(), Some(Rc::new(1)));
    }

    #[test]
    fn delete_min_removes_smallest() {
        let h = LeftistHeap::empty()
            .insert(&Rc::new(3))
            .insert(&Rc::new(1))
            .insert(&Rc::new(2));
        let h2 = h.delete_min().unwrap();
        assert_eq!(h2.find_min(), Some(Rc::new(2)));
    }

    #[test]
    fn sorted_order() {
        let h = LeftistHeap::empty()
            .insert(&Rc::new(5))
            .insert(&Rc::new(3))
            .insert(&Rc::new(8))
            .insert(&Rc::new(1))
            .insert(&Rc::new(4));
        assert_eq!(heap_to_sorted_vec(&h), vec![1, 3, 4, 5, 8]);
    }

    #[test]
    fn merge_two_heaps() {
        let a = LeftistHeap::empty()
            .insert(&Rc::new(5))
            .insert(&Rc::new(1))
            .insert(&Rc::new(3));
        let b = LeftistHeap::empty()
            .insert(&Rc::new(4))
            .insert(&Rc::new(2))
            .insert(&Rc::new(6));
        let merged = a.merge(&b);
        assert_eq!(heap_to_sorted_vec(&merged), vec![1, 2, 3, 4, 5, 6]);
    }

    #[test]
    fn merge_with_empty() {
        let a = LeftistHeap::empty().insert(&Rc::new(1)).insert(&Rc::new(2));
        let empty: LeftistHeap<i32> = LeftistHeap::empty();
        assert_eq!(heap_to_sorted_vec(&a.merge(&empty)), vec![1, 2]);
        assert_eq!(heap_to_sorted_vec(&empty.merge(&a)), vec![1, 2]);
    }

    #[test]
    fn persistent_insert() {
        let h0 = LeftistHeap::empty();
        let h1 = h0.insert(&Rc::new(2));
        let h2 = h1.insert(&Rc::new(1));
        assert_eq!(h1.find_min(), Some(Rc::new(2)));
        assert_eq!(h2.find_min(), Some(Rc::new(1)));
    }

    #[test]
    fn persistent_delete() {
        let h = LeftistHeap::empty().insert(&Rc::new(1)).insert(&Rc::new(2));
        let h2 = h.delete_min().unwrap();
        assert_eq!(h.find_min(), Some(Rc::new(1)));
        assert_eq!(h2.find_min(), Some(Rc::new(2)));
    }
}
