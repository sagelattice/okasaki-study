module type STACK = sig
  type 'a stack
  val empty    : 'a stack
  val isEmpty  : 'a stack -> bool
  val cons     : 'a -> 'a stack -> 'a stack
  val head     : 'a stack -> 'a
  val tail     : 'a stack -> 'a stack
end

module ListStack : STACK = struct
  type 'a stack = 'a list
  let empty  = []

  let isEmpty s = s = []
  let cons x s = x :: s

  let head = List.hd
  let tail = List.tl
end

module CustomStack : STACK = struct
  type 'a stack = Nil | Cons of 'a * 'a stack

  let empty = Nil

  let isEmpty s = s = Nil
  let cons x s = Cons (x, s)

  let head = function Cons(x, _) -> x | Nil -> failwith "head"
  let tail = function Cons(_, s) -> s | Nil -> failwith "tail"
end

module StackOps (S : STACK) = struct
  open S

  exception SUBSCRIPT

  let rec ( ++ ) xs ys =
    if isEmpty xs then ys
    else cons (head xs) (tail xs ++ ys)

  let rec update s i y =
    if isEmpty s then raise SUBSCRIPT
    else if i = 0 then cons y (tail s)
    else cons (head s) (update (tail s) (i - 1) y)
end

module TestStack (S: STACK) = struct
  module Ops = StackOps(S)
  let run () =
    let open S in
    let open Ops in
    let s = cons 1 (cons 2 (cons 3 empty)) in
    let t = cons 4 empty in
    assert (empty ++ s = s);
    assert (s ++ empty = s);
    assert (s ++ t = (cons 1 (cons 2 (cons 3 (cons 4 empty)))));
    assert (update s 0 0 = (cons 0 (cons 2 (cons 3 empty))));
    assert (update s 1 0 = (cons 1 (cons 0 (cons 3 empty))));
    assert (try ignore (update s 4 0); false with SUBSCRIPT -> true);
    (* no mutation! *)
    assert (s = cons 1 (cons 2 (cons 3 empty)));
    print_endline "passed"
end

let () =
  let module T1 = TestStack(ListStack) in
  let module T2 = TestStack(CustomStack) in
  T1.run ();
  T2.run ()

let () = print_endline "all tests passed"

