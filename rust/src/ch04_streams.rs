use std::cell::LazyCell;
use std::rc::Rc;

type LazyInit<'a, T> = Box<dyn FnOnce() -> T + 'a>;
struct Suspension<'a, T>(LazyCell<T, LazyInit<'a, T>>);

impl<'a, T: 'a> Suspension<'a, T> {
    fn new(f: impl FnOnce() -> T + 'a) -> Self {
        Suspension(LazyCell::new(Box::new(f)))
    }

    fn done(t: T) -> Self {
        Suspension(LazyCell::new(Box::new(move || t)))
    }

    fn force(&self) -> &T {
        LazyCell::force(&self.0)
    }
}

pub enum StreamCell<'a, T> {
    Nil,
    Cons(Rc<T>, Stream<'a, T>),
}

use StreamCell::*;

pub struct Stream<'a, T>(Rc<Suspension<'a, StreamCell<'a, T>>>);

impl<'a, T> Clone for Stream<'a, T> {
    fn clone(&self) -> Self {
        Stream(Rc::clone(&self.0))
    }
}

impl<'a, T: 'a> Stream<'a, T> {
    pub fn nil() -> Self {
        Stream(Rc::new(Suspension::done(Nil)))
    }

    pub fn force(&self) -> &StreamCell<'a, T> {
        self.0.force()
    }

    pub fn lazy(f: impl FnOnce() -> StreamCell<'a, T> + 'a) -> Self {
        Stream(Rc::new(Suspension::new(f)))
    }

    pub fn cons(&self, head: &Rc<T>) -> Self {
        Stream(Rc::new(Suspension::done(Cons(
            Rc::clone(head),
            self.clone(),
        ))))
    }

    pub fn take(&self, n: usize) -> Self {
        match (n, self.force()) {
            (0, _) => Self::nil(),
            (_, Nil) => Self::nil(),
            (n, Cons(x, s)) => {
                let x = Rc::clone(x);
                let s = s.clone();
                Self::lazy(move || Cons(x, s.take(n - 1)))
            }
        }
    }

    pub fn drop(&self, n: usize) -> Self {
        let mut cur = self.clone();
        for _ in 0..n {
            let next = match cur.force() {
                Nil => Self::nil(),
                Cons(_, s) => s.clone(),
            };
            cur = next;
        }
        cur
    }

    pub fn reverse(&self) -> Self {
        let mut acc = Self::nil();
        let mut cur = self.clone();
        loop {
            let (x, rest) = match cur.force() {
                Nil => return acc,
                Cons(x, rest) => (Rc::clone(x), rest.clone()),
            };
            cur = rest;
            let prev = acc;
            acc = Self::lazy(move || Cons(x, prev));
        }
    }

    pub fn append(&self, other: &Self) -> Self {
        match self.force() {
            Nil => other.clone(),
            Cons(x, s) => {
                let x = Rc::clone(x);
                let s = s.clone();
                let other = other.clone();
                Self::lazy(move || Cons(x, s.append(&other)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::StreamCell::*;
    use super::*;
    use std::cell::Cell;

    type S = Stream<'static, i32>;

    fn s(xs: &[i32]) -> S {
        xs.iter()
            .rev()
            .fold(S::nil(), |acc, &x| acc.cons(&Rc::new(x)))
    }

    fn to_vec(s: &S) -> Vec<i32> {
        let mut out = Vec::new();
        let mut cur = s.clone();
        loop {
            let next = match cur.force() {
                Nil => return out,
                Cons(x, rest) => {
                    out.push(**x);
                    rest.clone()
                }
            };
            cur = next;
        }
    }

    #[test]
    fn nil_is_empty() {
        assert!(matches!(S::nil().force(), Nil));
    }

    #[test]
    fn cons_produces_head() {
        let xs = S::nil().cons(&Rc::new(1));
        match xs.force() {
            Cons(x, _) => assert_eq!(**x, 1),
            Nil => panic!("expected Cons"),
        }
    }

    #[test]
    fn helper_round_trip() {
        assert_eq!(to_vec(&s(&[1, 2, 3])), vec![1, 2, 3]);
    }

    #[test]
    fn take_zero_is_empty() {
        assert!(to_vec(&s(&[1, 2, 3]).take(0)).is_empty());
    }

    #[test]
    fn take_n_returns_first_n() {
        assert_eq!(to_vec(&s(&[1, 2, 3, 4]).take(2)), vec![1, 2]);
    }

    #[test]
    fn take_more_than_length() {
        assert_eq!(to_vec(&s(&[1, 2]).take(5)), vec![1, 2]);
    }

    #[test]
    fn take_of_nil() {
        assert!(to_vec(&S::nil().take(3)).is_empty());
    }

    #[test]
    fn drop_zero_unchanged() {
        assert_eq!(to_vec(&s(&[1, 2, 3]).drop(0)), vec![1, 2, 3]);
    }

    #[test]
    fn drop_n_skips_first_n() {
        assert_eq!(to_vec(&s(&[1, 2, 3, 4]).drop(2)), vec![3, 4]);
    }

    #[test]
    fn drop_more_than_length() {
        assert!(to_vec(&s(&[1, 2]).drop(5)).is_empty());
    }

    #[test]
    fn drop_of_nil() {
        assert!(to_vec(&S::nil().drop(3)).is_empty());
    }

    #[test]
    fn reverse_empty() {
        assert!(to_vec(&S::nil().reverse()).is_empty());
    }

    #[test]
    fn reverse_singleton() {
        assert_eq!(to_vec(&s(&[1]).reverse()), vec![1]);
    }

    #[test]
    fn reverse_multiple() {
        assert_eq!(to_vec(&s(&[1, 2, 3]).reverse()), vec![3, 2, 1]);
    }

    #[test]
    fn reverse_involutive() {
        let xs = s(&[1, 2, 3, 4, 5]);
        assert_eq!(to_vec(&xs.reverse().reverse()), to_vec(&xs));
    }

    #[test]
    fn append_nil_left() {
        assert_eq!(to_vec(&S::nil().append(&s(&[1, 2]))), vec![1, 2]);
    }

    #[test]
    fn append_nil_right() {
        assert_eq!(to_vec(&s(&[1, 2]).append(&S::nil())), vec![1, 2]);
    }

    #[test]
    fn append_two_streams() {
        assert_eq!(to_vec(&s(&[1, 2]).append(&s(&[3, 4]))), vec![1, 2, 3, 4]);
    }

    #[test]
    fn force_is_memoized() {
        let counter = Rc::new(Cell::new(0u32));
        let counter_clone = Rc::clone(&counter);
        let xs: S = Stream::lazy(move || {
            counter_clone.set(counter_clone.get() + 1);
            Cons(Rc::new(42), Stream::nil())
        });
        xs.force();
        xs.force();
        xs.force();
        assert_eq!(counter.get(), 1, "closure should only run once");
    }
}
