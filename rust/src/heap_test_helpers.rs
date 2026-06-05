use crate::sigs::Heap;
use std::rc::Rc;

fn r(x: i32) -> Rc<i32> {
    Rc::new(x)
}

pub fn drain<H: Heap<Element = i32> + Clone>(h: &H) -> Vec<i32> {
    let mut result = Vec::new();
    let mut current = h.clone();
    while let Some(min) = current.find_min() {
        result.push(*min);
        current = current.delete_min().unwrap();
    }
    result
}

pub fn empty_is_empty<H: Heap<Element = i32>>() {
    let h = H::empty();
    assert!(h.is_empty());
    assert!(h.find_min().is_none());
    assert!(h.delete_min().is_none());
}

pub fn nonempty_after_insert<H: Heap<Element = i32>>() {
    assert!(!H::empty().insert(&r(1)).is_empty());
}

pub fn findmin_singleton<H: Heap<Element = i32>>() {
    assert_eq!(H::empty().insert(&r(42)).find_min(), Some(r(42)));
}

pub fn findmin_three<H: Heap<Element = i32>>() {
    let h = H::empty().insert(&r(3)).insert(&r(1)).insert(&r(2));
    assert_eq!(h.find_min(), Some(r(1)));
}

pub fn deletemin_exposes_next<H: Heap<Element = i32>>() {
    let h = H::empty().insert(&r(3)).insert(&r(1)).insert(&r(2));
    assert_eq!(h.delete_min().unwrap().find_min(), Some(r(2)));
}

pub fn merge_min<H: Heap<Element = i32>>() {
    let h1 = H::empty().insert(&r(3)).insert(&r(1));
    let h2 = H::empty().insert(&r(4)).insert(&r(2));
    assert_eq!(h1.merge(&h2).find_min(), Some(r(1)));
}

pub fn merge_second_min<H: Heap<Element = i32>>() {
    let h1 = H::empty().insert(&r(3)).insert(&r(1));
    let h2 = H::empty().insert(&r(4)).insert(&r(2));
    assert_eq!(h1.merge(&h2).delete_min().unwrap().find_min(), Some(r(2)));
}

pub fn merge_new_min<H: Heap<Element = i32>>() {
    let h1 = H::empty().insert(&r(3)).insert(&r(2));
    let h2 = H::empty().insert(&r(0));
    assert_eq!(h1.merge(&h2).find_min(), Some(r(0)));
}

pub fn findmin_empty_is_none<H: Heap<Element = i32>>() {
    assert!(H::empty().find_min().is_none());
}

pub fn deletemin_empty_is_none<H: Heap<Element = i32>>() {
    assert!(H::empty().delete_min().is_none());
}

pub fn immutability_insert<H: Heap<Element = i32>>() {
    let h = H::empty().insert(&r(1));
    let _ = h.insert(&r(0));
    assert_eq!(h.find_min(), Some(r(1)));
}

pub fn immutability_deletemin<H: Heap<Element = i32>>() {
    let h = H::empty().insert(&r(2)).insert(&r(1));
    let _ = h.delete_min().unwrap();
    assert_eq!(h.find_min(), Some(r(1)));
}

pub fn sorted_extraction<H: Heap<Element = i32> + Clone>() {
    let h = [5, 3, 1, 4, 2]
        .into_iter()
        .fold(H::empty(), |acc, x| acc.insert(&r(x)));
    assert_eq!(drain(&h), vec![1, 2, 3, 4, 5]);
}

pub fn merge_all_elements<H: Heap<Element = i32> + Clone>() {
    let h1 = H::empty().insert(&r(3)).insert(&r(1));
    let h2 = H::empty().insert(&r(4)).insert(&r(2));
    assert_eq!(drain(&h1.merge(&h2)), vec![1, 2, 3, 4]);
}

pub fn merge_with_empty<H: Heap<Element = i32> + Clone>() {
    let a = H::empty().insert(&r(1)).insert(&r(2));
    let e = H::empty();
    assert_eq!(drain(&a.merge(&e)), vec![1, 2]);
    assert_eq!(drain(&e.merge(&a)), vec![1, 2]);
}

pub fn repeated_equal_elements<H: Heap<Element = i32>>() {
    let h = H::empty().insert(&r(1)).insert(&r(1)).insert(&r(1));
    assert_eq!(h.find_min(), Some(r(1)));
    assert_eq!(h.delete_min().unwrap().find_min(), Some(r(1)));
}

// A heap is a multiset: every inserted element, including duplicates, is
// retained and extractable. Draining must yield each key with its exact
// multiplicity, so a bug that drops one of several equal elements is caught.
pub fn duplicates_all_retained<H: Heap<Element = i32> + Clone>() {
    let h = [1, 1, 2, 1]
        .into_iter()
        .fold(H::empty(), |acc, x| acc.insert(&r(x)));
    assert_eq!(drain(&h), vec![1, 1, 1, 2]);
}

pub fn merge_preserves_duplicates<H: Heap<Element = i32> + Clone>() {
    let h1 = H::empty().insert(&r(2)).insert(&r(1)).insert(&r(2));
    let h2 = H::empty().insert(&r(1)).insert(&r(3)).insert(&r(1));
    assert_eq!(drain(&h1.merge(&h2)), vec![1, 1, 1, 2, 2, 3]);
}

pub fn delete_min_removes_single_duplicate<H: Heap<Element = i32> + Clone>() {
    let h = H::empty().insert(&r(1)).insert(&r(1)).insert(&r(2));
    assert_eq!(drain(&h.delete_min().unwrap()), vec![1, 2]);
}

macro_rules! heap_tests {
    ($heap:ty) => {
        #[test]
        fn empty_is_empty() {
            $crate::heap_test_helpers::empty_is_empty::<$heap>();
        }
        #[test]
        fn nonempty_after_insert() {
            $crate::heap_test_helpers::nonempty_after_insert::<$heap>();
        }
        #[test]
        fn findmin_singleton() {
            $crate::heap_test_helpers::findmin_singleton::<$heap>();
        }
        #[test]
        fn findmin_three() {
            $crate::heap_test_helpers::findmin_three::<$heap>();
        }
        #[test]
        fn deletemin_exposes_next() {
            $crate::heap_test_helpers::deletemin_exposes_next::<$heap>();
        }
        #[test]
        fn merge_min() {
            $crate::heap_test_helpers::merge_min::<$heap>();
        }
        #[test]
        fn merge_second_min() {
            $crate::heap_test_helpers::merge_second_min::<$heap>();
        }
        #[test]
        fn merge_new_min() {
            $crate::heap_test_helpers::merge_new_min::<$heap>();
        }
        #[test]
        fn findmin_empty_is_none() {
            $crate::heap_test_helpers::findmin_empty_is_none::<$heap>();
        }
        #[test]
        fn deletemin_empty_is_none() {
            $crate::heap_test_helpers::deletemin_empty_is_none::<$heap>();
        }
        #[test]
        fn immutability_insert() {
            $crate::heap_test_helpers::immutability_insert::<$heap>();
        }
        #[test]
        fn immutability_deletemin() {
            $crate::heap_test_helpers::immutability_deletemin::<$heap>();
        }
        #[test]
        fn sorted_extraction() {
            $crate::heap_test_helpers::sorted_extraction::<$heap>();
        }
        #[test]
        fn merge_all_elements() {
            $crate::heap_test_helpers::merge_all_elements::<$heap>();
        }
        #[test]
        fn merge_with_empty() {
            $crate::heap_test_helpers::merge_with_empty::<$heap>();
        }
        #[test]
        fn repeated_equal_elements() {
            $crate::heap_test_helpers::repeated_equal_elements::<$heap>();
        }
        #[test]
        fn duplicates_all_retained() {
            $crate::heap_test_helpers::duplicates_all_retained::<$heap>();
        }
        #[test]
        fn merge_preserves_duplicates() {
            $crate::heap_test_helpers::merge_preserves_duplicates::<$heap>();
        }
        #[test]
        fn delete_min_removes_single_duplicate() {
            $crate::heap_test_helpers::delete_min_removes_single_duplicate::<$heap>();
        }
    };
}

pub(crate) use heap_tests;
