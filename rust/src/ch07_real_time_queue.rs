use crate::ch02_lists::List;
use crate::ch04_streams::{Stream, StreamCell};
use crate::sigs::Queue;
use std::rc::Rc;

pub struct RealTimeQueue<'a, T>(Stream<'a, T>, List<Rc<T>>, Stream<'a, T>);

impl<'a, T> Clone for RealTimeQueue<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone(), self.2.clone())
    }
}

impl<'a, T: 'a> RealTimeQueue<'a, T> {
    fn rotate(&self) -> Stream<'a, T> {
        let Self(f, r, s) = self;
        match (f.force(), r.uncons()) {
            (StreamCell::Nil, Some((y, _))) => {
                let (y, s) = (y.clone(), s.clone());
                Stream::lazy(move || StreamCell::Cons(y, s))
            }
            (StreamCell::Cons(x, xs), Some((y, ys))) => {
                let (x, xs, y, ys, s) = (x.clone(), xs.clone(), y.clone(), ys.clone(), s.clone());
                Stream::lazy(move || {
                    StreamCell::Cons(
                        x,
                        Self(xs, ys, Stream::lazy(move || StreamCell::Cons(y, s))).rotate(),
                    )
                })
            }
            (StreamCell::Nil, None) | (StreamCell::Cons(_, _), None) => {
                unreachable!("impossible by invariant |r| = |f| + 1")
            }
        }
    }
    fn exec(&self) -> Self {
        let Self(f, r, s) = self;
        match s.force() {
            StreamCell::Cons(_, s) => Self(f.clone(), r.clone(), s.clone()),
            StreamCell::Nil => {
                let g = Self(f.clone(), r.clone(), Stream::nil()).rotate();
                Self(g.clone(), List::empty(), g.clone())
            }
        }
    }
}

impl<'a, T: 'a> Queue for RealTimeQueue<'a, T> {
    type Item = T;
    fn empty() -> Self {
        Self(Stream::nil(), List::empty(), Stream::nil())
    }
    fn is_empty(&self) -> bool {
        match self.0.force() {
            StreamCell::Nil => true,
            _ => false,
        }
    }
    fn snoc(&self, item: &Rc<Self::Item>) -> Self {
        let Self(f, r, s) = self;
        Self(f.clone(), List::cons(Rc::clone(item), r), s.clone()).exec()
    }
    fn head(&self) -> Option<Rc<Self::Item>> {
        match self.0.force() {
            StreamCell::Nil => None,
            StreamCell::Cons(x, _) => Some(Rc::clone(x)),
        }
    }
    fn tail(&self) -> Option<Self> {
        match self.0.force() {
            StreamCell::Nil => None,
            StreamCell::Cons(_, f) => {
                let Self(_, r, s) = self;
                Some(Self(f.clone(), r.clone(), s.clone()).exec())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::queue_test_helpers::queue_tests;

    queue_tests!(RealTimeQueue<'static, i32>);
}
