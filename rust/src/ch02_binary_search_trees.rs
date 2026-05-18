use crate::sigs::Set;
use std::rc::Rc;

#[derive(Clone)]
enum Tree<Elem> {
    E,
    T(Rc<Self>, Elem, Rc<Self>),
}

impl<T: Ord + Clone> Tree<T> {
    fn insert(&self, item: &T) -> Self {
        match self {
            Self::E => Self::T(Rc::new(Self::E), item.clone(), Rc::new(Self::E)),
            Self::T(left, y, right) => {
                if item < y {
                    Self::T(Rc::new(left.insert(item)), y.clone(), Rc::clone(right))
                } else if y < item {
                    Self::T(Rc::clone(left), y.clone(), Rc::new(right.insert(item)))
                } else {
                    self.clone()
                }
            }
        }
    }

    fn member(&self, item: &T) -> bool {
        match self {
            Self::E => false,
            Self::T(left, y, right) => {
                if item < y {
                    left.member(item)
                } else if y < item {
                    right.member(item)
                } else {
                    true
                }
            }
        }
    }
}

pub struct UnbalancedSet<T>(Rc<Tree<T>>);

impl<T: Ord + Clone> Set for UnbalancedSet<T> {
    type Element = T;
    fn empty() -> Self {
        UnbalancedSet(Rc::new(Tree::E))
    }
    fn insert(&self, item: &T) -> Self {
        UnbalancedSet(Rc::new(self.0.insert(item)))
    }

    fn member(&self, item: &Self::Element) -> bool {
        self.0.member(item)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type S = UnbalancedSet<i32>;

    fn set(xs: &[i32]) -> S {
        xs.iter().fold(S::empty(), |acc, x| acc.insert(x))
    }

    #[test]
    fn empty_has_no_members() {
        assert!(!S::empty().member(&1));
    }

    #[test]
    fn inserted_element_is_member() {
        assert!(set(&[1]).member(&1));
    }

    #[test]
    fn absent_element_is_not_member() {
        assert!(!set(&[1, 2, 3]).member(&4));
    }

    #[test]
    fn insert_is_persistent() {
        let s1 = set(&[1, 2, 3]);
        let s2 = s1.insert(&4);
        assert!(!s1.member(&4));
        assert!(s2.member(&4));
    }

    #[test]
    fn duplicate_insert_is_idempotent() {
        let s = set(&[1, 1, 1]);
        assert!(s.member(&1));
        assert!(!s.member(&2));
    }

    #[test]
    fn degenerate_left_spine() {
        let s = set(&[5, 4, 3, 2, 1]);
        for i in 1..=5 {
            assert!(s.member(&i));
        }
        assert!(!s.member(&6));
    }
}
