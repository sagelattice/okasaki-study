use crate::queue_test_helpers::drain;
use crate::sigs::Deque;
use std::rc::Rc;

fn r(x: i32) -> Rc<i32> {
    Rc::new(x)
}

fn drain_back<D: Deque<Item = i32> + Clone>(d: &D) -> Vec<i32> {
    let mut result = Vec::new();
    let mut current = d.clone();
    while let Some(x) = current.last() {
        result.push(*x);
        current = current.init().unwrap();
    }
    result
}

pub fn last_empty_is_none<D: Deque<Item = i32>>() {
    assert!(D::empty().last().is_none());
}

pub fn init_empty_is_none<D: Deque<Item = i32>>() {
    assert!(D::empty().init().is_none());
}

pub fn last_singleton_snoc<D: Deque<Item = i32>>() {
    assert_eq!(D::empty().snoc(&r(42)).last(), Some(r(42)));
}

pub fn last_singleton_cons<D: Deque<Item = i32>>() {
    assert_eq!(D::empty().cons(&r(42)).last(), Some(r(42)));
}

pub fn last_returns_last_snocced<D: Deque<Item = i32>>() {
    let d = D::empty().snoc(&r(1)).snoc(&r(2)).snoc(&r(3));
    assert_eq!(d.last(), Some(r(3)));
}

pub fn cons_prepends<D: Deque<Item = i32>>() {
    let d = D::empty().snoc(&r(2)).snoc(&r(3)).cons(&r(1));
    assert_eq!(d.head(), Some(r(1)));
}

pub fn cons_head_tail<D: Deque<Item = i32>>() {
    let d = D::empty().cons(&r(3)).cons(&r(2)).cons(&r(1));
    assert_eq!(d.head(), Some(r(1)));
    assert_eq!(d.tail().unwrap().head(), Some(r(2)));
}

pub fn init_removes_last<D: Deque<Item = i32>>() {
    let d = D::empty().snoc(&r(1)).snoc(&r(2)).snoc(&r(3));
    assert_eq!(d.init().unwrap().last(), Some(r(2)));
}

pub fn drain_back_order<D: Deque<Item = i32> + Clone>() {
    let d = [1, 2, 3, 4, 5]
        .into_iter()
        .fold(D::empty(), |acc, x| acc.snoc(&r(x)));
    assert_eq!(drain_back(&d), vec![5, 4, 3, 2, 1]);
}

pub fn front_and_back_agree_on_singleton<D: Deque<Item = i32>>() {
    let d = D::empty().snoc(&r(7));
    assert_eq!(d.head(), d.last());
}

pub fn cons_then_drain_forward<D: Deque<Item = i32> + Clone>() {
    let d = [1, 2, 3]
        .into_iter()
        .rev()
        .fold(D::empty(), |acc, x| acc.cons(&r(x)));
    assert_eq!(drain(&d), vec![1, 2, 3]);
}

pub fn immutability_cons<D: Deque<Item = i32>>() {
    let d = D::empty().snoc(&r(1));
    let _ = d.cons(&r(0));
    assert_eq!(d.head(), Some(r(1)));
}

pub fn immutability_init<D: Deque<Item = i32>>() {
    let d = D::empty().snoc(&r(1)).snoc(&r(2));
    let _ = d.init().unwrap();
    assert_eq!(d.last(), Some(r(2)));
}

macro_rules! deque_tests {
    ($deque:ty) => {
        $crate::queue_test_helpers::queue_tests!($deque);

        #[test]
        fn last_empty_is_none() {
            $crate::deque_test_helpers::last_empty_is_none::<$deque>();
        }
        #[test]
        fn init_empty_is_none() {
            $crate::deque_test_helpers::init_empty_is_none::<$deque>();
        }
        #[test]
        fn last_singleton_snoc() {
            $crate::deque_test_helpers::last_singleton_snoc::<$deque>();
        }
        #[test]
        fn last_singleton_cons() {
            $crate::deque_test_helpers::last_singleton_cons::<$deque>();
        }
        #[test]
        fn last_returns_last_snocced() {
            $crate::deque_test_helpers::last_returns_last_snocced::<$deque>();
        }
        #[test]
        fn cons_prepends() {
            $crate::deque_test_helpers::cons_prepends::<$deque>();
        }
        #[test]
        fn cons_head_tail() {
            $crate::deque_test_helpers::cons_head_tail::<$deque>();
        }
        #[test]
        fn init_removes_last() {
            $crate::deque_test_helpers::init_removes_last::<$deque>();
        }
        #[test]
        fn drain_back_order() {
            $crate::deque_test_helpers::drain_back_order::<$deque>();
        }
        #[test]
        fn front_and_back_agree_on_singleton() {
            $crate::deque_test_helpers::front_and_back_agree_on_singleton::<$deque>();
        }
        #[test]
        fn cons_then_drain_forward() {
            $crate::deque_test_helpers::cons_then_drain_forward::<$deque>();
        }
        #[test]
        fn immutability_cons() {
            $crate::deque_test_helpers::immutability_cons::<$deque>();
        }
        #[test]
        fn immutability_init() {
            $crate::deque_test_helpers::immutability_init::<$deque>();
        }
    };
}

pub(crate) use deque_tests;
