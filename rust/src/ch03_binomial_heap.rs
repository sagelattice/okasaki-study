use crate::ch02_lists::List;
use crate::sigs::Heap;
use std::rc::Rc;

struct Node<T> {
    rank: usize,
    element: Rc<T>,
    children: TreeList<T>,
}

impl<T> Clone for Node<T> {
    fn clone(&self) -> Self {
        Self {
            rank: self.rank,
            element: Rc::clone(&self.element),
            children: self.children.clone(),
        }
    }
}

type TreeList<T> = List<Node<T>>;

impl<T: Ord> Node<T> {
    fn new(rank: usize, element: &Rc<T>, children: TreeList<T>) -> Self {
        Self {
            rank,
            element: Rc::clone(element),
            children,
        }
    }

    fn link(&self, other: &Self) -> Self {
        if self.element <= other.element {
            Self::new(
                self.rank + 1,
                &self.element,
                List::cons(other.clone(), &self.children),
            )
        } else {
            Self::new(
                self.rank + 1,
                &other.element,
                List::cons(self.clone(), &other.children),
            )
        }
    }
}

impl<T: Ord> TreeList<T> {
    fn ins_tree(&self, t: Node<T>) -> TreeList<T> {
        match self.uncons() {
            None => List::cons(t, &List::empty()),
            Some((h, rest)) => {
                if t.rank <= h.rank {
                    List::cons(t, self)
                } else {
                    rest.ins_tree(t.link(h))
                }
            }
        }
    }

    fn merge(&self, ts2: &TreeList<T>) -> TreeList<T> {
        match (self.uncons(), ts2.uncons()) {
            (None, _) => ts2.clone(),
            (_, None) => self.clone(),
            (Some((t1, rest1)), Some((t2, rest2))) => {
                if t1.rank < t2.rank {
                    List::cons(t1.clone(), &rest1.merge(ts2))
                } else if t2.rank < t1.rank {
                    List::cons(t2.clone(), &self.merge(rest2))
                } else {
                    rest1.merge(rest2).ins_tree(t1.link(t2))
                }
            }
        }
    }

    fn remove_min_tree(&self) -> Option<(Node<T>, Self)> {
        let (t, rest) = self.uncons()?;
        if rest.is_empty() {
            Some((t.clone(), rest.clone()))
        } else {
            let (u, urest) = rest.remove_min_tree()?;
            if t.element <= u.element {
                Some((t.clone(), rest.clone()))
            } else {
                Some((u, List::cons(t.clone(), &urest)))
            }
        }
    }
}

#[derive(Clone)]
pub struct BinomialHeap<T>(TreeList<T>);

impl<T: Ord> Heap for BinomialHeap<T> {
    type Element = T;

    fn empty() -> Self {
        BinomialHeap(List::empty())
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn insert(&self, item: &Rc<Self::Element>) -> Self {
        BinomialHeap(self.0.ins_tree(Node::new(0, item, List::empty())))
    }

    fn merge(&self, other: &Self) -> Self {
        BinomialHeap(self.0.merge(&other.0))
    }

    fn find_min(&self) -> Option<Rc<Self::Element>> {
        self.0.remove_min_tree().map(|(t, _)| Rc::clone(&t.element))
    }

    fn delete_min(&self) -> Option<Self> {
        self.0
            .remove_min_tree()
            .map(|(node, ts)| BinomialHeap(node.children.rev().merge(&ts)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap_test_helpers::heap_tests;

    heap_tests!(BinomialHeap<i32>);
}
