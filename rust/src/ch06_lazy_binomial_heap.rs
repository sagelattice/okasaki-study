use crate::ch02_lists::List;
use crate::ch04_streams::Suspension;
use crate::sigs::Heap;
use std::rc::Rc;

struct Node<T> {
    rank: usize,
    element: Rc<T>,
    children: TreeList<T>,
}

impl<T: Ord> Node<T> {
    fn link(s: &Rc<Node<T>>, t: &Rc<Node<T>>) -> Rc<Self> {
        if s.element <= t.element {
            Rc::new(Self {
                rank: s.rank + 1,
                element: Rc::clone(&s.element),
                children: List::cons(Rc::clone(t), &s.children),
            })
        } else {
            Rc::new(Self {
                rank: s.rank + 1,
                element: Rc::clone(&t.element),
                children: List::cons(Rc::clone(s), &t.children),
            })
        }
    }
}

type TreeList<T> = List<Rc<Node<T>>>;

impl<T: Ord> TreeList<T> {
    fn ins_tree(&self, node: &Rc<Node<T>>) -> Self {
        match self.uncons() {
            None => List::cons(Rc::clone(node), &List::empty()),
            Some((s, rest)) => {
                if node.rank < s.rank {
                    List::cons(Rc::clone(node), &List::cons(Rc::clone(s), &rest))
                } else {
                    rest.ins_tree(&Node::link(node, s))
                }
            }
        }
    }
    fn merge(&self, other: &Self) -> Self {
        match (self.uncons(), other.uncons()) {
            (_, None) => self.clone(),
            (None, _) => other.clone(),
            (Some((s, ss)), Some((t, ts))) => {
                if s.rank < t.rank {
                    List::cons(Rc::clone(s), &ss.merge(other))
                } else if t.rank < s.rank {
                    List::cons(Rc::clone(t), &self.merge(ts))
                } else {
                    ss.merge(ts).ins_tree(&Node::link(&s, &t))
                }
            }
        }
    }
    fn remove_min_tree(&self) -> Option<(Rc<Node<T>>, Self)> {
        match self.uncons() {
            None => None,
            Some((t, ts)) if ts.is_empty() => Some((Rc::clone(t), List::empty())),
            Some((t, ts)) => {
                let (s, ss) = ts.remove_min_tree()?;
                if t.element <= s.element {
                    Some((Rc::clone(t), ts.clone()))
                } else {
                    Some((s, List::cons(Rc::clone(t), &ss)))
                }
            }
        }
    }
}

pub struct LazyBinomialHeap<'a, T>(Rc<Suspension<'a, TreeList<T>>>);

impl<'a, T> Clone for LazyBinomialHeap<'a, T> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl<'a, T: Ord + 'a> Heap for LazyBinomialHeap<'a, T> {
    type Element = T;
    fn empty() -> Self {
        Self(Rc::new(Suspension::done(List::empty())))
    }
    fn is_empty(&self) -> bool {
        self.0.force().is_empty()
    }
    fn insert(&self, item: &Rc<Self::Element>) -> Self {
        let ts = Rc::clone(&self.0);
        let item = Rc::clone(item);
        Self(Rc::new(Suspension::lazy(move || {
            ts.force().ins_tree(&Rc::new(Node {
                rank: 0,
                element: item,
                children: List::empty(),
            }))
        })))
    }
    fn merge(&self, other: &Self) -> Self {
        let s = Rc::clone(&self.0);
        let t = Rc::clone(&other.0);
        Self(Rc::new(Suspension::lazy(move || {
            s.force().merge(t.force())
        })))
    }
    fn find_min(&self) -> Option<Rc<Self::Element>> {
        let (t, _) = self.0.force().remove_min_tree()?;
        Some(Rc::clone(&t.element))
    }
    fn delete_min(&self) -> Option<Self> {
        let (n, ts2) = self.0.force().remove_min_tree()?;
        let ts1 = n.children.clone();
        let ts2 = ts2.clone();
        Some(Self(Rc::new(Suspension::lazy(move || {
            ts1.rev().merge(&ts2)
        }))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap_test_helpers::heap_tests;

    heap_tests!(LazyBinomialHeap<'static, i32>);
}
