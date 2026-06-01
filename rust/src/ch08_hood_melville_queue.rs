use crate::ch02_lists::List;
use crate::sigs::Queue;
use std::rc::Rc;

enum RotationState<T> {
    Idle,
    Reversing(usize, List<Rc<T>>, List<Rc<T>>, List<Rc<T>>, List<Rc<T>>),
    Appending(usize, List<Rc<T>>, List<Rc<T>>),
    Done(List<Rc<T>>),
}

impl<T> Clone for RotationState<T> {
    fn clone(&self) -> Self {
        use RotationState::*;
        match self {
            Idle => Idle,
            Reversing(s, f, f_prime, r, r_prime) => {
                Reversing(*s, f.clone(), f_prime.clone(), r.clone(), r_prime.clone())
            }
            Appending(s, f_prime, r_prime) => Appending(*s, f_prime.clone(), r_prime.clone()),
            Done(newf) => Done(newf.clone()),
        }
    }
}

impl<T> RotationState<T> {
    fn exec(&self) -> Self {
        use RotationState::*;
        match self {
            Reversing(ok, f, f_prime, r, r_prime) => match (f.uncons(), r.uncons()) {
                (Some((x, f)), Some((y, r))) => Reversing(
                    ok + 1,
                    f.clone(),
                    List::cons(Rc::clone(x), f_prime),
                    r.clone(),
                    List::cons(Rc::clone(y), r_prime),
                ),
                (None, Some((y, r))) if r.is_empty() => {
                    Appending(*ok, f_prime.clone(), List::cons(Rc::clone(y), r_prime))
                }
                _ => self.clone(),
            },
            Appending(0, _, r_prime) => Done(r_prime.clone()),
            Appending(ok, f_prime, r_prime) => match f_prime.uncons() {
                Some((x, f_prime)) => {
                    Appending(ok - 1, f_prime.clone(), List::cons(Rc::clone(x), r_prime))
                }
                None => self.clone(),
            },
            _ => self.clone(),
        }
    }
    fn invalidate(&self) -> Self {
        use RotationState::*;
        match self {
            Reversing(ok, f, f_prime, r, r_prime) => Reversing(
                ok - 1,
                f.clone(),
                f_prime.clone(),
                r.clone(),
                r_prime.clone(),
            ),
            Appending(0, _, r_prime) => Done(r_prime.clone()),
            Appending(ok, f_prime, r_prime) => Appending(ok - 1, f_prime.clone(), r_prime.clone()),
            _ => self.clone(),
        }
    }
}

pub struct HoodMelvilleQueue<T> {
    lenf: usize,
    f: List<Rc<T>>,
    state: RotationState<T>,
    lenr: usize,
    r: List<Rc<T>>,
}

impl<T> Clone for HoodMelvilleQueue<T> {
    fn clone(&self) -> Self {
        Self {
            lenf: self.lenf,
            f: self.f.clone(),
            state: self.state.clone(),
            lenr: self.lenr,
            r: self.r.clone(),
        }
    }
}

impl<T> HoodMelvilleQueue<T> {
    fn exec2(&self) -> Self {
        match self.state.exec().exec() {
            RotationState::Done(newf) => Self {
                lenf: self.lenf,
                f: newf,
                state: RotationState::Idle,
                lenr: self.lenr,
                r: self.r.clone(),
            },
            new_state => Self {
                lenf: self.lenf,
                f: self.f.clone(),
                state: new_state,
                lenr: self.lenr,
                r: self.r.clone(),
            },
        }
    }
    fn check(&self) -> Self {
        if self.lenr <= self.lenf {
            self.exec2()
        } else {
            let new_state = RotationState::Reversing(
                0,
                self.f.clone(),
                List::empty(),
                self.r.clone(),
                List::empty(),
            );
            let q = Self {
                lenf: self.lenf + self.lenr,
                f: self.f.clone(),
                state: new_state,
                lenr: 0,
                r: List::empty(),
            };
            q.exec2()
        }
    }
}

impl<T> Queue for HoodMelvilleQueue<T> {
    type Item = T;
    fn empty() -> Self {
        Self {
            lenf: 0,
            f: List::empty(),
            state: RotationState::Idle,
            lenr: 0,
            r: List::empty(),
        }
    }
    fn is_empty(&self) -> bool {
        self.lenf == 0
    }
    fn snoc(&self, item: &Rc<T>) -> Self {
        let q = Self {
            lenf: self.lenf,
            f: self.f.clone(),
            state: self.state.clone(),
            lenr: self.lenr + 1,
            r: List::cons(Rc::clone(item), &self.r),
        };
        q.check()
    }
    fn head(&self) -> Option<Rc<Self::Item>> {
        if self.f.is_empty() {
            None
        } else {
            self.f.head().map(Rc::clone)
        }
    }
    fn tail(&self) -> Option<Self> {
        if self.f.is_empty() {
            None
        } else {
            let f = self.f.tail()?;
            let q = Self {
                lenf: self.lenf - 1,
                f: f.clone(),
                state: self.state.invalidate(),
                lenr: self.lenr,
                r: self.r.clone(),
            };
            Some(q.check())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::queue_test_helpers::queue_tests;

    queue_tests!(HoodMelvilleQueue<i32>);
}
