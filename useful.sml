functor UsefulFn (Move : MOVE where type Card.slot = int) :
  sig
    type slot = Move.Card.slot
    val applyItoJ : slot -> slot -> Move.t list  (* apply slot i to slot j,
                                                    leaving result in slot i
                                                  *)
    val apply_to_literal : slot -> int -> Move.t list (* a[i] := a[i](n) *)
    val loadCompose : slot -> Move.t list (* load the compose function into slot j
                                           *)
    val copyItoJ : slot -> slot -> Move.t list (* copy slot i to slot j *)

    val makeId : slot -> Move.t list  (* put I in the slot *)
    val loadN : slot -> int -> Move.t list   (* load N into the slot *)

    val mutualHelpUsing : slot -> slot -> slot -> int -> Move.t list
    val mutualTmps : { code : slot, t1 : slot, t2 : slot, t3 : slot } ->
        slot -> slot -> int -> Move.t list
    val mutualTmp : { code : slot, tN : slot } -> slot -> slot -> int -> Move.t list

    val attack : slot -> int -> int -> int -> Move.t list
    val help   : slot -> int -> int -> int -> Move.t list

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

  fun makeId slot = [M.CardToSlot (C.cast C.put, slot)]

  fun loadCompose slot = makeId slot @ G.loadTerm slot Terms.compose

  fun loadN slot n = makeId slot @ G.loadTerm slot (Terms.num n)

  fun copyItoJ from to =
    loadN to from @ [M.CardToSlot (C.cast C.get, to)]

  fun apply_to_literal slot n =
    let fun acd card = M.CardToSlot (C.cast card, slot)
        fun asl card = M.SlotToCard (slot, C.cast card)
        fun toNum Z = [asl C.zero]
          | toNum (Apply (f, n)) = acd C.K :: acd C.S :: asl (opcard f) :: toNum n
    in  toNum (numeral n)
    end

  fun trip_using card slot i j n =
         ( makeId slot
         @ [Move.SlotToCard (slot, C.cast card)]
         @ apply_to_literal slot i
         @ apply_to_literal slot j
         @ apply_to_literal slot n
         )

  val attack = trip_using C.attack
  val help   = trip_using C.help

  fun mutualHelpUsing slot i j n = help slot i j n @ help slot j i n
  fun mutualTmps { code : slot, t1 : slot, t2 : slot, t3 : slot } i j n =
     loadN t1 i @ loadN t2 j @ loadN t3 n @
     makeId code @
     [Move.SlotToCard (code, C.cast C.help)] @
     applyItoJ code t1 @ applyItoJ code t2 @ applyItoJ code t3 @
     [Move.SlotToCard (code, C.cast C.help)] @
     applyItoJ code t2 @ applyItoJ code t1 @ applyItoJ code t3
  fun mutualTmp { code : slot, tN : slot } i j n =
     loadN tN n @
     makeId code @
     [Move.SlotToCard (code, C.cast C.help)] @
     apply_to_literal code i @ apply_to_literal code j @ applyItoJ code tN @
     [Move.SlotToCard (code, C.cast C.help)] @
     apply_to_literal code j @ apply_to_literal code i @ applyItoJ code tN
end  
