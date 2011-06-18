functor Combo (Card : COMBINATORS where type 'a card = 'a Value.v) : sig
end =
struct
  structure C = Card
  val @@ = C.@@
  open Value

  infix 3 @@

  fun toFun' f = cast o toFun f o cast

  datatype a = A of a
  datatype b = B of b

  val fst : ('a -> 'b -> 'a) v = C.K
  val snd = F (fn v => toFun' (C.K @@ C.I : (a -> b -> b) v) v)

  fun self_I f = self_I f
        (* given f, return function g that when applied to I,
                    applies f to I, then returns g *)

  datatype 'exp combo = I | S | K | put | :@: of 'exp combo * 'exp combo | Exp of 'exp

  infix 9 :@:

  structure IR = struct
    datatype stm (* normally, we don't care what value is produced *)
      = Seq of stm * stm
      | Inc of slot
      | Dec of slot
      | Attack of slot * slot' * num
      | Help of slot * slot * num
      | Revive of slot
      | Zombie of slot * unit v
      | Self of stm (* when executed, produces self *)
    and num
      = Zero | Succ of num | Dbl of num
      | GetN  of num
      | CopyN of num
      | Combo of num combo
    withtype slot = num
    and      slot' = num
  end

  (* assume a stm is represented as a term which, when applied to anything,
     has a side effect, and returns something unspecified (except for Self) *)
  (* assume an exp is represented as a term which, when evaluated,
     returns the value *)

  fun compose f g = (S :@: (K :@: f)) :@: ((S :@: (K :@: g)) :@: I)


(*
  fun stm (Seq (s1, s2)) =
*)      


end
