use crate::ch02_lists::List;
use crate::ch04_streams::{Stream, StreamCell};
use crate::sigs::Heap;
use std::rc::Rc;

struct Node<T>(Rc<T>, List<Rc<Self>>);

impl<T: Ord> Node<T> {
    fn link(s: &Rc<Self>, t: &Rc<Self>) -> Rc<Self> {
        let Self(x1, c1) = s.as_ref();
        let Self(x2, c2) = t.as_ref();
        if x1 <= x2 {
            Rc::new(Self(Rc::clone(x1), List::cons(Rc::clone(t), c1)))
        } else {
            Rc::new(Self(Rc::clone(x2), List::cons(Rc::clone(s), c2)))
        }
    }
}

enum Digit<T> {
    Zero,
    One(Rc<Node<T>>),
}

impl<T> Digit<T> {
    fn is_zero(&self) -> bool {
        match self {
            Self::Zero => true,
            _ => false,
        }
    }
}

type DigitStream<'a, T> = Stream<'a, Digit<T>>;

impl<'a, T: 'a + Ord> DigitStream<'a, T> {
    fn merge(&self, other: &Self) -> Self {
        match (self.force(), other.force()) {
            (_, StreamCell::Nil) => self.clone(),
            (StreamCell::Nil, _) => other.clone(),
            (StreamCell::Cons(d, ds), StreamCell::Cons(e, es)) => match (d.as_ref(), e.as_ref()) {
                (_, Digit::Zero) => {
                    let (d, ds, es) = (Rc::clone(d), ds.clone(), es.clone());
                    Stream::lazy(move || StreamCell::Cons(d, ds.merge(&es)))
                }
                (Digit::Zero, _) => {
                    let (e, ds, es) = (Rc::clone(e), ds.clone(), es.clone());
                    Stream::lazy(move || StreamCell::Cons(e, ds.merge(&es)))
                }
                (Digit::One(x), Digit::One(y)) => {
                    let (x, y, ds, es) = (Rc::clone(x), Rc::clone(y), ds.clone(), es.clone());
                    Stream::lazy(move || {
                        StreamCell::Cons(
                            Rc::new(Digit::Zero),
                            ds.merge(&es).ins_tree(&Node::link(&x, &y)),
                        )
                    })
                }
            },
        }
    }
    fn normalize(&self) -> Self {
        let mut cur = self.clone();
        loop {
            match cur.force() {
                StreamCell::Nil => break,
                StreamCell::Cons(_, ds) => cur = ds.clone(),
            }
        }
        self.clone()
    }
    fn ins_tree(&self, node: &Rc<Node<T>>) -> Self {
        let node = Rc::clone(node);
        match self.force() {
            StreamCell::Nil => {
                Self::lazy(move || StreamCell::Cons(Rc::new(Digit::One(node)), Self::nil()))
            }
            StreamCell::Cons(d, ds) => match d.as_ref() {
                Digit::Zero => {
                    let ds = ds.clone();
                    Self::lazy(move || StreamCell::Cons(Rc::new(Digit::One(node)), ds))
                }
                Digit::One(t_prime) => {
                    let ds = ds.clone();
                    let node = node.clone();
                    let t_prime = Rc::clone(t_prime);
                    Self::lazy(move || {
                        StreamCell::Cons(
                            Rc::new(Digit::Zero),
                            ds.ins_tree(&Node::link(&node, &t_prime)),
                        )
                    })
                }
            },
        }
    }
    fn remove_min_tree(&self) -> Option<(Rc<Node<T>>, Self)> {
        match self.force() {
            StreamCell::Nil => None,
            StreamCell::Cons(d, ds) => match (d.as_ref(), ds.force()) {
                (Digit::One(t), StreamCell::Nil) => Some((Rc::clone(t), Stream::nil())),
                (Digit::Zero, _) => {
                    let (t, ds_prime) = ds.remove_min_tree()?;
                    Some((
                        t,
                        Stream::lazy(move || StreamCell::Cons(Rc::new(Digit::Zero), ds_prime)),
                    ))
                }
                (Digit::One(t), _) => {
                    let (u, ds_prime) = ds.remove_min_tree()?;
                    let Node(x, _) = t.as_ref();
                    let Node(y, _) = u.as_ref();
                    let t = Rc::clone(t);
                    if x <= y {
                        let ds = ds.clone();
                        Some((
                            t,
                            Stream::lazy(move || StreamCell::Cons(Rc::new(Digit::Zero), ds)),
                        ))
                    } else {
                        Some((
                            u,
                            Stream::lazy(move || {
                                StreamCell::Cons(Rc::new(Digit::One(t)), ds_prime)
                            }),
                        ))
                    }
                }
            },
        }
    }
}

struct Schedule<'a, T>(List<DigitStream<'a, T>>);

impl<'a, T: 'a> Schedule<'a, T> {
    fn exec(&self) -> Self {
        match self.0.uncons() {
            None => Self(List::empty()),
            Some((s, sched)) => match s.force() {
                StreamCell::Cons(d, job) if d.is_zero() => Self(List::cons(job.clone(), sched)),
                _ => Self(sched.clone()),
            },
        }
    }
}

pub struct ScheduledBinomialHeap<'a, T>(DigitStream<'a, T>, Schedule<'a, T>);

impl<'a, T: 'a + Ord> Heap for ScheduledBinomialHeap<'a, T> {
    type Element = T;
    fn empty() -> Self {
        Self(Stream::nil(), Schedule(List::empty()))
    }
    fn is_empty(&self) -> bool {
        match self.0.force() {
            StreamCell::Nil => true,
            StreamCell::Cons(_, _) => false,
        }
    }
    fn insert(&self, item: &Rc<Self::Element>) -> Self {
        let Self(ds, sched) = self;
        let ds = ds.ins_tree(&Rc::new(Node(Rc::clone(item), List::empty())));
        Self(ds.clone(), Schedule(List::cons(ds, &sched.0)).exec().exec())
    }
    fn merge(&self, other: &Self) -> Self {
        let ds = self.0.merge(&other.0);
        Self(ds, Schedule(List::empty()))
    }
    fn find_min(&self) -> Option<Rc<Self::Element>> {
        let (n, _) = self.0.remove_min_tree()?;
        Some(Rc::clone(&n.0))
    }
    fn delete_min(&self) -> Option<Self> {
        let (n, ds) = self.0.remove_min_tree()?;
        let ds = ds.merge(
            &n.1.rev()
                .into_iter()
                .map(|t| Rc::new(Digit::One(t)))
                .collect(),
        );
        Some(Self(ds.normalize(), Schedule(List::empty())))
    }
}

impl<'a, T> Clone for ScheduledBinomialHeap<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), Schedule(self.1.0.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap_test_helpers::heap_tests;

    heap_tests!(ScheduledBinomialHeap<'static, i32>);
}
