use std::cell::LazyCell;
use std::rc::Rc;

type LazyInit<'a, T> = Box<dyn FnOnce() -> T + 'a>;
pub struct Suspension<'a, T>(LazyCell<T, LazyInit<'a, T>>);

impl<'a, T: 'a> Suspension<'a, T> {
    pub fn lazy(f: impl FnOnce() -> T + 'a) -> Self {
        Suspension(LazyCell::new(Box::new(f)))
    }

    pub fn done(t: T) -> Self {
        Suspension(LazyCell::new(Box::new(move || t)))
    }

    pub fn force(&self) -> &T {
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
        Stream(Rc::new(Suspension::lazy(f)))
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
        let s = self.clone();
        Self::lazy(move || {
            let mut acc: StreamCell<'a, T> = Nil;
            let mut cur = s;
            loop {
                match cur.force() {
                    Nil => return acc,
                    Cons(x, rest) => {
                        let x = Rc::clone(x);
                        cur = rest.clone();
                        let prev = std::mem::replace(&mut acc, Nil);
                        acc = Cons(x, Self::lazy(move || prev));
                    }
                }
            }
        })
    }

    pub fn append(&self, other: &Self) -> Self {
        let s = self.clone();
        let other = other.clone();
        Self::lazy(move || match s.force() {
            Nil => match other.force() {
                Nil => Nil,
                Cons(x, rest) => Cons(Rc::clone(x), rest.clone()),
            },
            Cons(x, rest) => Cons(Rc::clone(x), rest.append(&other)),
        })
    }
}

impl<'a, T: 'a> FromIterator<Rc<T>> for Stream<'a, T> {
    fn from_iter<I: IntoIterator<Item = Rc<T>>>(iter: I) -> Stream<'a, T> {
        iter.into_iter()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .fold(Stream::nil(), |acc, x| acc.cons(&x))
    }
}

impl<'a, T: 'a> IntoIterator for Stream<'a, T> {
    type Item = Rc<T>;
    type IntoIter = StreamIter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        StreamIter { cur: self }
    }
}

pub struct StreamIter<'a, T> {
    cur: Stream<'a, T>,
}

impl<'a, T: 'a> Iterator for StreamIter<'a, T> {
    type Item = Rc<T>;
    fn next(&mut self) -> Option<Self::Item> {
        let (head, tail) = match self.cur.force() {
            Nil => return None,
            Cons(x, rest) => (Rc::clone(x), rest.clone()),
        };
        self.cur = tail;
        Some(head)
    }
}

#[cfg(test)]
mod tests {
    use super::StreamCell::*;
    use super::*;
    use std::cell::Cell;

    type S = Stream<'static, i32>;

    fn s(xs: &[i32]) -> S {
        xs.iter().map(|&x| Rc::new(x)).collect()
    }

    fn to_vec(s: &S) -> Vec<i32> {
        s.clone().into_iter().map(|rc| *rc).collect()
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
    fn from_iter_then_into_iter_round_trip() {
        assert_eq!(to_vec(&s(&[1, 2, 3])), vec![1, 2, 3]);
    }

    #[test]
    fn collect_from_arbitrary_iterator() {
        let xs: S = (1..=3).map(Rc::new).collect();
        assert_eq!(to_vec(&xs), vec![1, 2, 3]);
    }

    #[test]
    fn for_loop_visits_each_element() {
        let xs = s(&[1, 2, 3]);
        let mut collected = Vec::new();
        for x in xs.clone() {
            collected.push(*x);
        }
        assert_eq!(collected, vec![1, 2, 3]);
    }

    #[test]
    fn into_iter_empty() {
        assert_eq!(S::nil().into_iter().count(), 0);
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
