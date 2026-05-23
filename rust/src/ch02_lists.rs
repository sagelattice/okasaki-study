use std::rc::Rc;

pub trait Stack<T>: Sized {
    fn empty() -> Self;
    fn is_empty(&self) -> bool;
    fn cons(head: T, tail: &Self) -> Self;
    fn head(&self) -> Option<&T>;
    fn tail(&self) -> Option<&Self>;
    fn uncons(&self) -> Option<(&T, &Self)>;
    fn rev(&self) -> Self;
}

#[derive(Clone, PartialEq, Debug)]
struct Node<T> {
    head: T,
    tail: CustomStack<T>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CustomStack<T>(Option<Rc<Node<T>>>);

impl<T: Clone> Stack<T> for CustomStack<T> {
    fn empty() -> Self {
        CustomStack(None)
    }

    fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    fn cons(head: T, tail: &Self) -> Self {
        CustomStack(Some(Rc::new(Node {
            head,
            tail: tail.clone(),
        })))
    }

    fn head(&self) -> Option<&T> {
        self.0.as_ref().map(|node| &node.head)
    }

    fn tail(&self) -> Option<&Self> {
        self.0.as_ref().map(|node| &node.tail)
    }

    fn uncons(&self) -> Option<(&T, &Self)> {
        self.0.as_ref().map(|node| (&node.head, &node.tail))
    }

    fn rev(&self) -> Self {
        let mut result = Self::empty();
        let mut current = self;
        loop {
            match current.head() {
                None => break result,
                Some(h) => {
                    result = Self::cons(h.clone(), &result);
                    current = current.tail().unwrap();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type S = CustomStack<i32>;

    fn s(xs: &[i32]) -> S {
        xs.iter().rev().fold(S::empty(), |acc, &x| S::cons(x, &acc))
    }

    #[test]
    fn empty_is_empty() {
        assert!(S::empty().is_empty());
    }

    #[test]
    fn cons_is_not_empty() {
        assert!(!s(&[1]).is_empty());
    }

    #[test]
    fn head_of_empty_is_none() {
        assert_eq!(S::empty().head(), None);
    }

    #[test]
    fn tail_of_empty_is_none() {
        assert_eq!(S::empty().tail(), None);
    }

    #[test]
    fn head_returns_first_element() {
        assert_eq!(s(&[1, 2, 3]).head(), Some(&1));
    }

    #[test]
    fn tail_returns_rest() {
        assert_eq!(s(&[1, 2, 3]).tail(), Some(&s(&[2, 3])));
    }

    #[test]
    fn cons_same_tail_twice() {
        let tail = s(&[2, 3]);
        let s1 = S::cons(1, &tail);
        let s2 = S::cons(9, &tail);
        assert_eq!(s1.tail(), s2.tail());
    }
}
