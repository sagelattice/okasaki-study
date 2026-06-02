use crate::ch04_streams::{Stream, StreamCell};
use crate::sigs::{Deque, Queue};
use std::rc::Rc;

impl<'a, T: 'a> Stream<'a, T> {
    fn exec1(&self) -> Self {
        match self.force() {
            StreamCell::Cons(_, s) => s.clone(),
            StreamCell::Nil => self.clone(),
        }
    }
    fn exec2(&self) -> Self {
        self.exec1().exec1()
    }
    fn rotate_rev<const C: usize>(&self, r: &Self, a: Stream<'a, T>) -> Self {
        match self.force() {
            StreamCell::Nil => r.reverse().append(&a),
            StreamCell::Cons(x, f) => {
                let (x, f, r) = (Rc::clone(x), f.clone(), r.clone());
                Self::lazy(move || {
                    StreamCell::Cons(
                        x,
                        f.rotate_rev::<C>(&r.drop(C), r.take(C).reverse().append(&a)),
                    )
                })
            }
        }
    }
    fn rotate_drop<const C: usize>(&self, j: usize, r: &Self) -> Self {
        if j < C {
            self.rotate_rev::<C>(&r.drop(j), Stream::nil())
        } else {
            match self.force() {
                StreamCell::Cons(x, f_prime) => {
                    let (x, f_prime, r) = (Rc::clone(x), f_prime.clone(), r.clone());
                    Self::lazy(move || {
                        StreamCell::Cons(x, f_prime.rotate_drop::<C>(j - C, &r.drop(C)))
                    })
                }
                StreamCell::Nil => unreachable!("|r| = j + C * |f|, therefore |f| >= 1"),
            }
        }
    }
}

pub struct RealTimeDeque<'a, T, const C: usize> {
    lenf: usize,
    f: Stream<'a, T>,
    sf: Stream<'a, T>,
    lenr: usize,
    r: Stream<'a, T>,
    sr: Stream<'a, T>,
}

impl<'a, T: 'a, const C: usize> Clone for RealTimeDeque<'a, T, C> {
    fn clone(&self) -> Self {
        Self {
            lenf: self.lenf,
            f: self.f.clone(),
            sf: self.sf.clone(),
            lenr: self.lenr,
            r: self.r.clone(),
            sr: self.sr.clone(),
        }
    }
}

impl<'a, T: 'a, const C: usize> RealTimeDeque<'a, T, C> {
    fn check(&self) -> Self {
        let &Self {
            lenf,
            ref f,
            sf: _,
            lenr,
            ref r,
            sr: _,
        } = self;
        if lenf > (C * lenr) + 1 {
            let i = (lenf + lenr) / 2;
            let j = lenf + lenr - i;
            let f_prime = f.take(i);
            let r_prime = r.rotate_drop::<C>(i, f);
            Self {
                lenf: i,
                f: f_prime.clone(),
                sf: f_prime,
                lenr: j,
                r: r_prime.clone(),
                sr: r_prime,
            }
        } else if lenr > (C * lenf) + 1 {
            let j = (lenf + lenr) / 2;
            let i = lenf + lenr - j;
            let r_prime = r.take(j);
            let f_prime = f.rotate_drop::<C>(j, r);
            Self {
                lenf: i,
                f: f_prime.clone(),
                sf: f_prime,
                lenr: j,
                r: r_prime.clone(),
                sr: r_prime,
            }
        } else {
            self.clone()
        }
    }
}

impl<'a, T: 'a, const C: usize> Queue for RealTimeDeque<'a, T, C> {
    type Item = T;
    fn empty() -> Self {
        Self {
            lenf: 0,
            f: Stream::nil(),
            sf: Stream::nil(),
            lenr: 0,
            r: Stream::nil(),
            sr: Stream::nil(),
        }
    }
    fn is_empty(&self) -> bool {
        (self.lenf + self.lenr) == 0
    }
    fn snoc(&self, item: &Rc<Self::Item>) -> Self {
        let (x, r) = (Rc::clone(item), self.r.clone());
        let q = Self {
            lenf: self.lenf,
            f: self.f.clone(),
            sf: self.sf.exec1(),
            lenr: self.lenr + 1,
            r: Stream::lazy(move || StreamCell::Cons(x, r)),
            sr: self.sr.exec1(),
        };
        q.check()
    }
    fn head(&self) -> Option<Rc<Self::Item>> {
        match (self.f.force(), self.r.force()) {
            (StreamCell::Nil, StreamCell::Nil) => None,
            (StreamCell::Nil, StreamCell::Cons(x, _)) | (StreamCell::Cons(x, _), _) => {
                Some(Rc::clone(x))
            }
        }
    }
    fn tail(&self) -> Option<Self> {
        match (self.f.force(), self.r.force()) {
            (StreamCell::Nil, StreamCell::Nil) => None,
            (StreamCell::Nil, StreamCell::Cons(_, _)) => Some(Self::empty()),
            (StreamCell::Cons(_, f_prime), _) => {
                let q = Self {
                    lenf: self.lenf - 1,
                    f: f_prime.clone(),
                    sf: self.sf.exec2(),
                    lenr: self.lenr,
                    r: self.r.clone(),
                    sr: self.sr.exec2(),
                };
                Some(q.check())
            }
        }
    }
}

impl<'a, T: 'a, const C: usize> Deque for RealTimeDeque<'a, T, C> {
    fn cons(&self, item: &Rc<Self::Item>) -> Self {
        let (x, f) = (Rc::clone(item), self.f.clone());
        let q = Self {
            lenf: self.lenf + 1,
            f: Stream::lazy(move || StreamCell::Cons(x, f)),
            sf: self.sf.exec1(),
            lenr: self.lenr,
            r: self.r.clone(),
            sr: self.sr.exec1(),
        };
        q.check()
    }
    fn last(&self) -> Option<Rc<Self::Item>> {
        match (self.f.force(), self.r.force()) {
            (StreamCell::Nil, StreamCell::Nil) => None,
            (StreamCell::Cons(x, _), StreamCell::Nil) | (_, StreamCell::Cons(x, _)) => {
                Some(Rc::clone(x))
            }
        }
    }
    fn init(&self) -> Option<Self> {
        match (self.f.force(), self.r.force()) {
            (StreamCell::Nil, StreamCell::Nil) => None,
            (StreamCell::Cons(_, _), StreamCell::Nil) => Some(Self::empty()),
            (_, StreamCell::Cons(_, r_prime)) => {
                let q = Self {
                    lenf: self.lenf,
                    f: self.f.clone(),
                    sf: self.sf.exec2(),
                    lenr: self.lenr - 1,
                    r: r_prime.clone(),
                    sr: self.sr.exec2(),
                };
                Some(q.check())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::deque_test_helpers::deque_tests;

    deque_tests!(RealTimeDeque<i32, 3>);
}
