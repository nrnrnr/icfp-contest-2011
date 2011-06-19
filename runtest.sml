(*
functor RunTermFn(structure Move : MOVE
                 structure Tx : CARD_TRANSLATE
                     where type 'a C1.card = 'a Move.Card.card
                       and type 'a C2.card = 'a Term.card
                 val player1 : Move.t option -> Move.t
                 val player2 : Move.t option -> Move.t) = 
struct
  type vitality = int

  structure Run1 = RunFn
      (structure Value = Value
       type vitality = int
       val this_clock = clock
       val automatic = automatic
       val proponent = slots1
       val opponent = slots2)

  structure Run2 = RunFn
      (structure Value = Value
       type vitality = int
       val this_clock = clock
       val automatic = automatic
       val proponent = slots2
       val opponent = slots1)

  datatype player = P of { getmove : Move.t option -> Move.t
                         , slots   : (vitality * unit Value.v) array
                         , cs : Move.Card.unitype Value.v -> int -> unit
                         , sc : int -> Move.Card.unitype Value.v -> unit
                         }

  val p1 = P { getmove = player1
             , slots   = slots1
             , cs = Run1.cardToSlot o Value.cast
             , sc = (fn s => fn c => Run1.slotToCard s (Value.cast c))
             }

  val p2 = P { getmove = player2
             , slots   = slots2
             , cs = Run2.cardToSlot o Value.cast
             , sc = (fn s => fn c => Run2.slotToCard s (Value.cast c))
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

                   
*)

structure M = struct val x =99 end
