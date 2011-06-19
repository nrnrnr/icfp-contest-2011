signature SIMULATOR = sig
  (* simulates one and only one game, destructively *)
  structure Move : MOVE
  type field
  type vitality = int
  datatype player = P1 | P2
  val otherplayer : player -> player
  type slots = (vitality * field) array
  datatype state =  (* all elements should be treated as read only *)
      S of { slots1 : slots
           , slots2 : slots
           , whose_turn : player ref
           , lastmove   : Move.t option ref
           , half_turns_remaining : int ref
           }
  val game      : state (* P1 always moves first *)
  val step      : Move.t -> unit  (* updates the current 'game' state *)
  val skip_turn : unit -> unit    (* current player skips a turn *)
  val game_over : unit -> bool
  val length_in_full_turns : int (* length of a game in full turns *)
end      


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

(*

              structure Tx : CARD_TRANSLATE
                      where type 'a C1.card = 'a Move.Card.card
                        and type 'a C2.card = 'a Term.card
                  val player1 : Move.t option -> Move.t
                  val player2 : Move.t option -> Move.t) :
*)

functor SimFn(val debug : bool) : SIMULATOR 
 = 
struct
  structure T = Term
  structure Move = MoveFn(TermCard)
  type vitality = int
  type field = Unitype.t Term.t
  type slots = (vitality * field) array
  val I = Term.C Term.I

  datatype player = P1 | P2
  datatype state =
      S of { slots1 : slots
           , slots2 : slots
           , whose_turn : player ref
           , lastmove   : Move.t option ref
           , half_turns_remaining : int ref
           }
  val length_in_full_turns = 100000  

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


  datatype playerinfo = P of 
                         { slots   : (vitality * field) array
                         , cs : Unitype.t Term.card -> int -> unit
                         , sc : int -> Unitype.t Term.card -> unit
                         }

  val game as S record = S { slots1 = VMs.slots1
                           , slots2 = VMs.slots2
                           , whose_turn = ref P1
                           , lastmove   = ref NONE
                           , half_turns_remaining = ref (2 * length_in_full_turns)
                           }

  val game = S record

  val p1 = P { slots   = #slots1 record
             , cs = I1.cardToSlot
             , sc = I1.slotToCard
             }

  val p2 = P { slots   = #slots2 record
             , cs = I2.cardToSlot
             , sc = I2.slotToCard
             }

  fun otherplayer P1 = P2
    | otherplayer P2 = P1


  fun allDead slots = Array.all (fn (v, _) => v <= 0) slots

  fun game_over () =
    ! (#half_turns_remaining record) = 0 orelse
    allDead (#slots1 record) orelse allDead (#slots2 record)

  fun advance_turn () =
    let val whose_turn = #whose_turn record
        val htr = #half_turns_remaining record
        val _ = htr := !htr - 1
        val _ = whose_turn := otherplayer (!whose_turn)
    in  ()
    end


  fun step move =
    let (* run the zombies *)
        val _ = VMs.automatic := true
        val whose_turn = #whose_turn record
        val P player = case !whose_turn of P1 => p1 | P2 => p2
        val slots = #slots player
        fun runZombie slot = #sc player slot Term.I
        fun run i =
          let val (v, f) = Array.sub (slots, i)
              val _ = if v = ~1 then runZombie i else ()
          in  if i = 255 then () else run (i + 1)
          end
        val _ = run 0
        val _ = VMs.automatic := false
        val _ = (case move
                   of Move.CardToSlot (c, slot) => #cs player c slot
                    | Move.SlotToCard (slot, c) => #sc player slot c
                ) handle Value.Error _ => ()
                       | Subscript => ()
                       | e => raise e  (* not good for production *)
        val lastmove = #lastmove record
        val _ = lastmove := SOME move
        val _ = advance_turn ()
    in  ()
    end

  fun skip_turn () =
    let val lastmove = #lastmove record
        val _ = lastmove := NONE
        val _ = advance_turn ()
    in  ()
    end



(*

  fun runAuto (P player) =
    let val _ = VMs.automatic := true
        val slots = #slots player
        fun runZombie slot = #sc player slot Term.I
        fun run i =
          let val (v, f) = Array.sub (slots, i)
              val _ = if v = ~1 then runZombie i else ()
          in  if i = 255 then () else run (i + 1)
          end
        val _ = run 0
        val _ = VMs.automatic := false
    in  ()
    end


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

  fun run () = halfturn (2 * total_turns) NONE p1 p2
*)
end
