use crate::sigs::Queue;
use std::rc::Rc;

fn r(x: i32) -> Rc<i32> {
    Rc::new(x)
}

pub fn drain<Q: Queue<Item = i32> + Clone>(q: &Q) -> Vec<i32> {
    let mut result = Vec::new();
    let mut current = q.clone();
    while let Some(x) = current.head() {
        result.push(*x);
        current = current.tail().unwrap();
    }
    result
}

pub fn empty_is_empty<Q: Queue<Item = i32>>() {
    let q = Q::empty();
    assert!(q.is_empty());
    assert!(q.head().is_none());
    assert!(q.tail().is_none());
}

pub fn nonempty_after_snoc<Q: Queue<Item = i32>>() {
    assert!(!Q::empty().snoc(&r(1)).is_empty());
}

pub fn head_singleton<Q: Queue<Item = i32>>() {
    assert_eq!(Q::empty().snoc(&r(42)).head(), Some(r(42)));
}

pub fn head_returns_first_snocced<Q: Queue<Item = i32>>() {
    let q = Q::empty().snoc(&r(1)).snoc(&r(2)).snoc(&r(3));
    assert_eq!(q.head(), Some(r(1)));
}

pub fn tail_exposes_next<Q: Queue<Item = i32>>() {
    let q = Q::empty().snoc(&r(1)).snoc(&r(2)).snoc(&r(3));
    assert_eq!(q.tail().unwrap().head(), Some(r(2)));
}

pub fn head_empty_is_none<Q: Queue<Item = i32>>() {
    assert!(Q::empty().head().is_none());
}

pub fn tail_empty_is_none<Q: Queue<Item = i32>>() {
    assert!(Q::empty().tail().is_none());
}

pub fn immutability_snoc<Q: Queue<Item = i32>>() {
    let q = Q::empty().snoc(&r(1));
    let _ = q.snoc(&r(2));
    assert_eq!(q.head(), Some(r(1)));
}

pub fn immutability_tail<Q: Queue<Item = i32>>() {
    let q = Q::empty().snoc(&r(1)).snoc(&r(2));
    let _ = q.tail().unwrap();
    assert_eq!(q.head(), Some(r(1)));
}

pub fn fifo_drain<Q: Queue<Item = i32> + Clone>() {
    let q = [1, 2, 3, 4, 5]
        .into_iter()
        .fold(Q::empty(), |acc, x| acc.snoc(&r(x)));
    assert_eq!(drain(&q), vec![1, 2, 3, 4, 5]);
}

pub fn snoc_after_partial_drain<Q: Queue<Item = i32> + Clone>() {
    let q = Q::empty().snoc(&r(1)).snoc(&r(2)).snoc(&r(3));
    let q = q.tail().unwrap().tail().unwrap();
    let q = q.snoc(&r(4)).snoc(&r(5));
    assert_eq!(drain(&q), vec![3, 4, 5]);
}

pub fn drain_to_empty<Q: Queue<Item = i32>>() {
    let q = Q::empty().snoc(&r(1)).snoc(&r(2));
    let q = q.tail().unwrap().tail().unwrap();
    assert!(q.is_empty());
    assert!(q.head().is_none());
    assert!(q.tail().is_none());
}

pub fn repeated_equal_elements<Q: Queue<Item = i32>>() {
    let q = Q::empty().snoc(&r(1)).snoc(&r(1)).snoc(&r(1));
    assert_eq!(q.head(), Some(r(1)));
    assert_eq!(q.tail().unwrap().head(), Some(r(1)));
}

macro_rules! queue_tests {
    ($queue:ty) => {
        #[test]
        fn empty_is_empty() {
            $crate::queue_test_helpers::empty_is_empty::<$queue>();
        }
        #[test]
        fn nonempty_after_snoc() {
            $crate::queue_test_helpers::nonempty_after_snoc::<$queue>();
        }
        #[test]
        fn head_singleton() {
            $crate::queue_test_helpers::head_singleton::<$queue>();
        }
        #[test]
        fn head_returns_first_snocced() {
            $crate::queue_test_helpers::head_returns_first_snocced::<$queue>();
        }
        #[test]
        fn tail_exposes_next() {
            $crate::queue_test_helpers::tail_exposes_next::<$queue>();
        }
        #[test]
        fn head_empty_is_none() {
            $crate::queue_test_helpers::head_empty_is_none::<$queue>();
        }
        #[test]
        fn tail_empty_is_none() {
            $crate::queue_test_helpers::tail_empty_is_none::<$queue>();
        }
        #[test]
        fn immutability_snoc() {
            $crate::queue_test_helpers::immutability_snoc::<$queue>();
        }
        #[test]
        fn immutability_tail() {
            $crate::queue_test_helpers::immutability_tail::<$queue>();
        }
        #[test]
        fn fifo_drain() {
            $crate::queue_test_helpers::fifo_drain::<$queue>();
        }
        #[test]
        fn snoc_after_partial_drain() {
            $crate::queue_test_helpers::snoc_after_partial_drain::<$queue>();
        }
        #[test]
        fn drain_to_empty() {
            $crate::queue_test_helpers::drain_to_empty::<$queue>();
        }
        #[test]
        fn repeated_equal_elements() {
            $crate::queue_test_helpers::repeated_equal_elements::<$queue>();
        }
    };
}

pub(crate) use queue_tests;
