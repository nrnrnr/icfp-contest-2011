functor InstructionsFn (structure VM : VM
                        type 'a card
                        val apply : VM.field * VM.field -> VM.field
                        val card  : Unitype.t card -> VM.field
                        val I : VM.field
                       ) = struct
  val <=: = VM.<=:
  infix <=:
  fun cardToSlot c slot =
        slot <=: apply (card c, VM.ourLiveField slot) handle _ => slot <=: I
  fun slotToCard slot c =
        slot <=: apply (VM.ourLiveField slot, card c) handle _ => slot <=: I

  fun with_fresh_clock f x y = (Clock.reset VM.clock; f x y)
  val cardToSlot = fn x => with_fresh_clock cardToSlot x
  val slotToCard = fn y => with_fresh_clock slotToCard y
end


functor RunTermFn(structure Move : MOVE
                 structure Tx : CARD_TRANSLATE
                     where type 'a C1.card = 'a Move.Card.card
                       and type 'a C2.card = 'a Term.card
                 val player1 : Move.t option -> Move.t
                 val player2 : Move.t option -> Move.t) = 
struct
  structure T = Term
  type vitality = int
  type field = Unitype.t Term.t
  val I = Term.C Term.I
  fun untyped apply (f, x) = Term.castTerm (apply (Term.castTerm f, Term.castTerm x))

  structure VMs = VMPairFn(type field = field
                           val initialField = Term.C Term.I)
  structure C1 = TermCombinatorsFn(val clock = VMs.clock
                                   structure VM = VMs.Player1)

  structure C2 = TermCombinatorsFn(val clock = VMs.clock
                                   structure VM = VMs.Player2)

  structure I1 = InstructionsFn(structure VM = VMs.Player1
                                type 'a card = 'a Term.card
                                fun apply fx = untyped C1.@@ fx
                                val card  = C1.card
                                val I = I)

  structure I2 = InstructionsFn(structure VM = VMs.Player2
                                type 'a card = 'a Term.card
                                fun apply fx = untyped C2.@@ fx
                                val card  = C2.card
                                val I = I)

  datatype player = P of { getmove : Move.t option -> Move.t
                         , slots   : (vitality * field) array
                         , cs : Unitype.t Term.card -> int -> unit
                         , sc : int -> Unitype.t Term.card -> unit
                         }

  val p1 = P { getmove = player1
             , slots   = VMs.slots1
             , cs = I1.cardToSlot
             , sc = I1.slotToCard
             }

  val p2 = P { getmove = player2
             , slots   = VMs.slots2
             , cs = I2.cardToSlot
             , sc = I2.slotToCard
             }

  val tx = Tx.translate

  fun unP (P record) = record

  fun runAuto player = runAuto player (* not yet implemented *)
  fun allDead player = allDead player (* not yet implemented *)

  fun halfturn n lastmove player otherplayer =
    let val _ = runAuto player
        val move = #getmove (unP player) lastmove
        val _ = (case move
                   of Move.CardToSlot (c, slot) => #cs (unP player) (tx c) slot
                    | Move.SlotToCard (slot, c) => #sc (unP player) slot (tx c) 
                ) handle Value.Error _ => ()
                       | Subscript => ()
                       | e => raise e  (* not good for production *)
    in  if n = 0 orelse allDead player orelse allDead otherplayer then
          () (* need to report outcome here *)
        else        
          halfturn (n - 1) (SOME move) otherplayer player
    end

  val total_turns = 100000  
  fun run () = halfturn (2 * total_turns) NONE p1 p2
end
