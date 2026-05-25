use crate::ch02_lists::List;
use crate::sigs::Queue;
use std::rc::Rc;

pub struct BatchedQueue<T>(List<Rc<T>>, List<Rc<T>>);

impl<T> Clone for BatchedQueue<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl<T> BatchedQueue<T> {
    fn checkf(f: &List<Rc<T>>, r: &List<Rc<T>>) -> Self {
        if f.is_empty() {
            Self(r.rev(), List::empty())
        } else {
            Self(f.clone(), r.clone())
        }
    }
}

impl<T> Queue for BatchedQueue<T> {
    type Item = T;
    fn empty() -> Self {
        Self(List::empty(), List::empty())
    }
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    fn snoc(&self, item: &Rc<Self::Item>) -> Self {
        let Self(f, r) = self;
        Self::checkf(f, &List::cons(Rc::clone(item), &r))
    }
    fn head(&self) -> Option<Rc<Self::Item>> {
        self.0.uncons().map(|(x, _)| Rc::clone(x))
    }
    fn tail(&self) -> Option<Self> {
        let Self(_, r) = self;
        self.0.uncons().map(|(_, f)| Self::checkf(f, r))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::queue_test_helpers::queue_tests;

    queue_tests!(BatchedQueue<i32>);
}
