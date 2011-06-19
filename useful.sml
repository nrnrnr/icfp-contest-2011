functor Useful (Move : MOVE where type Card.slot = int) :
  sig
    type slot = Move.Card.slot
    val applyItoJ : slot -> slot -> Move.t list  (* apply slot i to slot j *)
  end =
struct
  type slot = Move.Card.slot
  structure C = Move.Card
  structure M = Move
  local
    datatype rator = S | D (* successor or double *)
    fun opcard S = C.succ
      | opcard D = C.dbl

    datatype numeral = Z | Apply of rator * numeral
    fun id n = n
    fun succ n = Apply (S, n)

    fun numeral 0 = Z
      | numeral n = (if odd n then succ else id) (double (numeral (n div 2)))
    and double Z = Z
      | double n = Apply (D, n)
    and odd n = n mod 2 = 1

        
  in
    fun applyItoJ i j = (* sequence of moves to apply slot i to slot j *)
      let fun acd card = M.CardToSlot (C.cast card, i)
          fun asl card = M.SlotToCard (i, C.cast card)
          fun toNum Z = [asl C.zero]
            | toNum (Apply (f, n)) = acd C.K :: acd C.S :: asl (opcard f) :: toNum n
      in  acd C.K :: acd C.S :: asl C.get :: toNum (numeral j)
      end
  end
end  
