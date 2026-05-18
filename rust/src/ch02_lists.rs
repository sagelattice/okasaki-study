use std::rc::Rc;

pub trait Stack<T>: Sized {
    fn empty() -> Self;
    fn is_empty(&self) -> bool;
    fn cons(head: T, tail: &Self) -> Self;
    fn head(&self) -> Option<&T>;
    fn tail(&self) -> Option<Self>;
}

#[derive(Clone, PartialEq, Debug)]
enum Node<T> {
    Nil,
    Cons(T, Rc<Self>),
}

#[derive(Debug, PartialEq)]
pub struct CustomStack<T>(Rc<Node<T>>);

impl<T> Stack<T> for CustomStack<T> {
    fn empty() -> Self {
        CustomStack(Rc::new(Node::Nil))
    }

    fn is_empty(&self) -> bool {
        matches!(*self.0, Node::Nil)
    }

    fn cons(head: T, tail: &Self) -> Self {
        CustomStack(Rc::new(Node::Cons(head, Rc::clone(&tail.0))))
    }

    fn head(&self) -> Option<&T> {
        match &*self.0 {
            Node::Nil => None,
            Node::Cons(x, _) => Some(x),
        }
    }

    fn tail(&self) -> Option<Self> {
        match &*self.0 {
            Node::Nil => None,
            Node::Cons(_, tail) => Some(CustomStack(Rc::clone(&tail))),
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
        assert_eq!(s(&[1, 2, 3]).tail(), Some(s(&[2, 3])));
    }

    #[test]
    fn cons_same_tail_twice() {
        let tail = s(&[2, 3]);
        let s1 = S::cons(1, &tail);
        let s2 = S::cons(9, &tail);
        assert_eq!(s1.tail(), s2.tail());
    }
}
