use crate::ch04_streams::{Stream, StreamCell};
use crate::sigs::{Deque, Queue};
use std::rc::Rc;

pub struct BankersDeque<'a, T, const C: usize> {
    lenf: usize,
    f: Stream<'a, T>,
    lenr: usize,
    r: Stream<'a, T>,
}

impl<'a, T, const C: usize> Clone for BankersDeque<'a, T, C> {
    fn clone(&self) -> Self {
        Self {
            lenf: self.lenf,
            f: self.f.clone(),
            lenr: self.lenr,
            r: self.r.clone(),
        }
    }
}

impl<'a, T: 'a, const C: usize> BankersDeque<'a, T, C> {
    fn check(&self) -> Self {
        let &Self {
            lenf,
            ref f,
            lenr,
            ref r,
        } = self;
        if lenf > (C * lenr) + 1 {
            let i = (lenf + lenr) / 2;
            let j = lenf + lenr - i;
            let f_prime = f.take(i);
            let r_prime = r.append(&f.drop(i).reverse());
            Self {
                lenf: i,
                f: f_prime,
                lenr: j,
                r: r_prime,
            }
        } else if lenr > (C * lenf) + 1 {
            let j = (lenf + lenr) / 2;
            let i = lenf + lenr - j;
            let r_prime = r.take(j);
            let f_prime = f.append(&r.drop(j).reverse());
            Self {
                lenf: i,
                f: f_prime,
                lenr: j,
                r: r_prime,
            }
        } else {
            self.clone()
        }
    }
}

impl<'a, T: 'a, const C: usize> Queue for BankersDeque<'a, T, C> {
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
        (self.lenf + self.lenr) == 0
    }
    fn snoc(&self, item: &Rc<Self::Item>) -> Self {
        let (x, r) = (Rc::clone(item), self.r.clone());
        let q = Self {
            lenf: self.lenf,
            f: self.f.clone(),
            lenr: self.lenr + 1,
            r: Stream::lazy(move || StreamCell::Cons(x, r)),
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
                    lenr: self.lenr,
                    r: self.r.clone(),
                };
                Some(q.check())
            }
        }
    }
}

impl<'a, T: 'a, const C: usize> Deque for BankersDeque<'a, T, C> {
    fn last(&self) -> Option<Rc<Self::Item>> {
        match (self.f.force(), self.r.force()) {
            (StreamCell::Nil, StreamCell::Nil) => None,
            (StreamCell::Cons(x, _), StreamCell::Nil) | (_, StreamCell::Cons(x, _)) => {
                Some(Rc::clone(x))
            }
        }
    }
    fn cons(&self, item: &Rc<Self::Item>) -> Self {
        let (x, f) = (Rc::clone(item), self.f.clone());
        let q = Self {
            lenf: self.lenf + 1,
            f: Stream::lazy(move || StreamCell::Cons(x, f)),
            lenr: self.lenr,
            r: self.r.clone(),
        };
        q.check()
    }
    fn init(&self) -> Option<Self> {
        match (self.f.force(), self.r.force()) {
            (StreamCell::Nil, StreamCell::Nil) => None,
            (StreamCell::Cons(_, _), StreamCell::Nil) => Some(Self::empty()),
            (_, StreamCell::Cons(_, r_prime)) => {
                let q = Self {
                    lenf: self.lenf,
                    f: self.f.clone(),
                    lenr: self.lenr - 1,
                    r: r_prime.clone(),
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

    deque_tests!(BankersDeque<'static, i32, 3>);
}
