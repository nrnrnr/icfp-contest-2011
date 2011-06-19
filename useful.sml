functor Useful (Move : MOVE where type Card.slot = int) :
  sig
    type slot = Move.Card.slot
    val applyItoJ : slot -> slot -> Move.t list  (* apply slot i to slot j,
                                                    leaving result in slot i
                                                  *)
    val apply_to_literal : slot -> int -> Move.t list (* a[i] := a[i](n) *)
    val loadCompose : slot -> Move.t list (* load the compose function into slot j,
                                             assuming it is initially the identity
                                           *)
    val copyItoJ : slot -> slot -> Move.t list (* copy slot i to slot j,
                                                  assuming j holds the identity *)
    val makeId : slot -> Move.t list  (* put I in the slot *)
    val loadN : slot -> int -> Move.t list   (* load N into the slot, assuming it holds I *)

  end =
struct
  type slot = Move.Card.slot
  structure C = Move.Card
  structure M = Move

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

        
    fun applyItoJ i j = (* sequence of moves to apply slot i to slot j *)
      let fun acd card = M.CardToSlot (C.cast card, i)
          fun asl card = M.SlotToCard (i, C.cast card)
          fun toNum Z = [asl C.zero]
            | toNum (Apply (f, n)) = acd C.K :: acd C.S :: asl (opcard f) :: toNum n
      in  acd C.K :: acd C.S :: asl C.get :: toNum (numeral j)
      end


  type card = Unitype.t C.card

  infix 3 :@> <@:

  structure G = struct
    datatype gen = :@> of card * gen
                 | <@: of gen * card
                 | C      of card
    fun cast (a :@> c) = (C.cast a :@> cast c)
      | cast (c <@: a) = (cast c <@: C.cast a)
      | cast (C c)     = C (C.cast c)

    fun loadTerm slot =
      let fun load' (C c) = [M.SlotToCard (slot, c)]
            | load' (c :@> t) = M.CardToSlot (c, slot) :: load' t
            | load' (t <@: c) = M.SlotToCard (slot, c) :: load' t
      in  rev o load'
      end

  end



  structure Terms = struct
    type 'a t = G.gen
    val C : 'a C.card -> 'a t = fn c => G.C (C.cast c)
    val op :@> : ('a -> 'b) C.card * 'a t -> 'b t =
      fn (x, y) => G.:@> (C.cast x, G.cast y)
    val op <@: : ('a -> 'b) t * 'a C.card -> 'b t =
      fn (x, y) => G.<@: (G.cast x, C.cast y)
   
    fun num n =
      let fun num Z = C C.zero
            | num (Apply (f, n)) = opcard f :@> num n
      in  num (numeral n)
      end

    val compose = (C.S :@> (C C.K <@: C.S)) <@: C.K : Unitype.t t
  end

  fun loadCompose slot = G.loadTerm slot Terms.compose

  fun makeId slot = [M.CardToSlot (C.cast C.put, slot)]

  fun loadN slot n = G.loadTerm slot (Terms.num n)

  fun copyItoJ from to =
    loadN to from @ [M.CardToSlot (C.cast C.get, to)]

  fun apply_to_literal slot n =
    let fun acd card = M.CardToSlot (C.cast card, slot)
        fun asl card = M.SlotToCard (slot, C.cast card)
        fun toNum Z = [asl C.zero]
          | toNum (Apply (f, n)) = acd C.K :: acd C.S :: asl (opcard f) :: toNum n
    in  toNum (numeral n)
    end

end  
