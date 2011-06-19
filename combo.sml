functor ComboFn (Combo : COMBINATORS where type 'a t = 'a Value.v) : sig
end =
struct
  structure C = Combo.Card
  val @@ = Combo.@@
  val card = Combo.card
  open Value

  infix 3 @@

  fun toFun' f = cast o toFun f o cast

  datatype a = A of a
  datatype b = B of b

  val fst = F (fn v => toFun' (card C.K : ('a -> 'b -> 'a) v) v)
  val snd = F (fn v => toFun' (card C.K @@ card C.I : (a -> b -> b) v) v)

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
      | Store of slot * num
    and num
      = Zero | Succ of num | Dbl of num
      | GetN  of num
      | CopyN of num
      | Combo of num combo
      | ESeq  of stm * num
    withtype slot = num
    and      slot' = num
  end

  (* assume a stm is represented as a term which, when applied to anything,
     has a side effect, and returns something unspecified (except for Self) *)
  (* assume an exp is represented as a term which, when evaluated,
     returns the value *)

  fun compose f g = (S :@: (K :@: f)) :@: ((S :@: (K :@: g)) :@: I)

  val compose' = S :@: (K :@: S) :@: K

  fun self f      = S :@: (K :@: (K :@: f)) :@: (S :@: (K :@: f) :@: I)

  fun self' f = (fn x => f) o f

  structure Test = struct
    fun self_compose_law f = self f = compose (K :@: f) f
    fun assert p = if p then () else let exception Assert in raise Assert end
    val _ = assert (self_compose_law (Exp 1))
  end

  fun stm (IR.Seq (s1, s2)) = compose (stm s2) (stm s1)
    | stm s = stm s

  fun id x = x


  fun literal 0 = IR.Zero
    | literal n = (if odd n then IR.Succ else id) (double (literal (n div 2)))
  and double IR.Zero = IR.Zero
    | double n = IR.Dbl n
  and odd n = n mod 2 = 1

end
