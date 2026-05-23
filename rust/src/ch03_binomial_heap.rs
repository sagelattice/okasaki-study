use crate::ch02_lists::{CustomStack, Stack};
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

type TreeList<T> = CustomStack<Node<T>>;

impl<T: PartialOrd> Node<T> {
    fn new(rank: usize, element: &Rc<T>, children: TreeList<T>) -> Self {
        Self {
            rank,
            element: Rc::clone(element),
            children,
        }
    }

    fn link(t1: Self, t2: Self) -> Self {
        if t1.element <= t2.element {
            Self::new(
                t1.rank + 1,
                &t1.element,
                CustomStack::cons(t2, &t1.children),
            )
        } else {
            Self::new(
                t1.rank + 1,
                &t2.element,
                CustomStack::cons(t1, &t2.children),
            )
        }
    }

    fn ins_tree(t: Self, ts: &TreeList<T>) -> TreeList<T> {
        match ts.uncons() {
            None => CustomStack::cons(t, &CustomStack::empty()),
            Some((h, rest)) => {
                if t.rank <= h.rank {
                    CustomStack::cons(t, ts)
                } else {
                    Self::ins_tree(Self::link(t, h.clone()), rest)
                }
            }
        }
    }

    fn merge(ts1: &TreeList<T>, ts2: &TreeList<T>) -> TreeList<T> {
        match (ts1.uncons(), ts2.uncons()) {
            (None, _) => ts2.clone(),
            (_, None) => ts1.clone(),
            (Some((t1, rest1)), Some((t2, rest2))) => {
                if t1.rank < t2.rank {
                    CustomStack::cons(t1.clone(), &Self::merge(rest1, ts2))
                } else if t2.rank < t1.rank {
                    CustomStack::cons(t2.clone(), &Self::merge(ts1, rest2))
                } else {
                    Self::ins_tree(
                        Self::link(t1.clone(), t2.clone()),
                        &Self::merge(rest1, rest2),
                    )
                }
            }
        }
    }
}

impl<T: PartialOrd> TreeList<T> {
    fn remove_min_tree(&self) -> Option<(Node<T>, Self)> {
        match self.uncons() {
            None => None,
            Some((t, rest)) => {
                if rest.is_empty() {
                    return Some((t.clone(), rest.clone()));
                }
                if let Some((u, urest)) = rest.remove_min_tree() {
                    if t.element <= u.element {
                        return Some((t.clone(), rest.clone()));
                    } else {
                        return Some((u, CustomStack::cons(t.clone(), &urest)));
                    }
                }
                None
            }
        }
    }
}

#[derive(Clone)]
pub struct BinomialHeap<T>(TreeList<T>);

impl<T: PartialOrd> Heap for BinomialHeap<T> {
    type Element = T;

    fn empty() -> Self {
        BinomialHeap(CustomStack::empty())
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn insert(&self, item: &Rc<Self::Element>) -> Self {
        BinomialHeap(Node::ins_tree(
            Node::new(0, item, CustomStack::empty()),
            &self.0,
        ))
    }

    fn merge(&self, other: &Self) -> Self {
        BinomialHeap(Node::merge(&self.0, &other.0))
    }

    fn find_min(&self) -> Option<Rc<Self::Element>> {
        self.0.remove_min_tree().map(|(t, _)| Rc::clone(&t.element))
    }

    fn delete_min(&self) -> Option<Self> {
        self.0
            .remove_min_tree()
            .map(|(node, ts)| BinomialHeap(Node::merge(&node.children.rev(), &ts)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap_test_helpers::heap_tests;

    heap_tests!(BinomialHeap<i32>);
}
