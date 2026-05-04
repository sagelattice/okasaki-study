module type STACK = sig
  type 'a stack

  val empty : 'a stack
  val isEmpty : 'a stack -> bool
  val cons : 'a -> 'a stack -> 'a stack
  val head : 'a stack -> 'a
  val tail : 'a stack -> 'a stack
end

module ListStack : STACK = struct
  type 'a stack = 'a list

  let empty = []
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
  let head = function Cons (x, _) -> x | Nil -> failwith "head"
  let tail = function Cons (_, s) -> s | Nil -> failwith "tail"
end

module StackOps (S : STACK) = struct
  open S

  exception SUBSCRIPT

  let rec ( ++ ) xs ys =
    if isEmpty xs then ys else cons (head xs) (tail xs ++ ys)

  let rec update s i y =
    if isEmpty s then raise SUBSCRIPT
    else if i = 0 then cons y (tail s)
    else cons (head s) (update (tail s) (i - 1) y)
end
