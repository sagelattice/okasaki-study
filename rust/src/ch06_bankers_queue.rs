use crate::ch04_streams::{Stream, StreamCell};
use crate::sigs::Queue;
use std::rc::Rc;

pub struct BankersQueue<'a, T> {
    lenf: usize,
    f: Stream<'a, T>,
    lenr: usize,
    r: Stream<'a, T>,
}

impl<'a, T> Clone for BankersQueue<'a, T> {
    fn clone(&self) -> Self {
        Self {
            lenf: self.lenf,
            f: self.f.clone(),
            lenr: self.lenr,
            r: self.r.clone(),
        }
    }
}

impl<'a, T: 'a> BankersQueue<'a, T> {
    fn check(&self) -> Self {
        let Self { lenf, f, lenr, r } = self;
        if lenr <= lenf {
            self.clone()
        } else {
            Self {
                lenf: lenf + lenr,
                f: f.append(&r.reverse()),
                lenr: 0,
                r: Stream::nil(),
            }
        }
    }
}

impl<'a, T: 'a> Queue for BankersQueue<'a, T> {
    type Item = T;
    fn empty() -> Self {
        Self {
            lenf: 0,
            f: Stream::nil(),
            lenr: 0,
            r: Stream::nil(),
        }
    }
    fn is_empty(&self) -> bool {
        self.lenf == 0
    }
    fn snoc(&self, item: &Rc<Self::Item>) -> Self {
        let Self { lenf, f, lenr, r } = self;
        let q = Self {
            lenf: *lenf,
            f: f.clone(),
            lenr: lenr + 1,
            r: r.cons(item),
        };
        q.check()
    }
    fn head(&self) -> Option<Rc<T>> {
        match self.f.force() {
            StreamCell::Nil => None,
            StreamCell::Cons(x, _) => Some(Rc::clone(x)),
        }
    }
    fn tail(&self) -> Option<Self> {
        let Self { lenf, f, lenr, r } = self;
        match f.force() {
            StreamCell::Nil => None,
            StreamCell::Cons(_, s) => {
                let q = Self {
                    lenf: lenf - 1,
                    f: s.clone(),
                    lenr: *lenr,
                    r: r.clone(),
                };
                Some(q.check())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::queue_test_helpers::queue_tests;

    queue_tests!(BankersQueue<'static, i32>);
}
