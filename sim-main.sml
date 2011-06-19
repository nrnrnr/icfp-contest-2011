functor SimMainFn(structure Sim : SIMULATOR
                      where type 'a Move.Card.card = 'a TermCard.card
                        and type field = TermCard.field
                  structure Move : MOVE where type 'a Card.card = string
                  structure Tx : CARD_TRANSLATE
                      where type 'a C1.card = 'a Move.Card.card
                        and type 'a C2.card = 'a TermCard.card) :
    sig
        val runAlt : unit -> unit   (* players 1 and 2 alternate *)
        val runOnly : unit -> unit  (* player 1 runs alone *)
    end
  =
struct
    structure IO = MoveIOFn(structure Move = Move
                            val prompt = true)

    structure TxMove = TxMoveFn(structure M1 = Move
                                structure M2 = Sim.Move
                                structure Tx = Tx)
    structure S = Sim

    val printMove = IO.printMove
    val getMove = IO.readMove

    fun playerString S.P1 = "player 0"
      | playerString S.P2 = "player 1"

    val game as Sim.S record = Sim.game

    val (slots1, slots2) = (#slots1 record, #slots2 record)

    fun whichTurn () =
      let val half_turns_used =
                 2 * Sim.length_in_full_turns - ! (#half_turns_remaining record)
      in  1 + half_turns_used div 2
      end

    fun whose_turn () = ! (#whose_turn record)

    fun takeTurn skip =
      if skip (whose_turn ()) then
        (Sim.skip_turn (); takeTurn skip)
      else
        let val turn = whichTurn ()
            val player = whose_turn()
            val slots = case player of S.P1 => slots1 | S.P2 => slots2
            val _ = case player
                      of S.P1 => app print ["###### turn ", Int.toString turn, "\n"]
                       | S.P2 => ()
            val _ = app print ["*** ", playerString player, "'s turn, with slots:\n",
                               "(slots {10000,I} are omitted)\n"]
            val _ = Dump.slots slots
            val move = getMove (playerString player)
            val _ = printMove (playerString player) move
            val move = TxMove.translate move
            val _ = Sim.step move
        in  ()
        end

    fun run skip =
      if Sim.game_over () then ()
      else (takeTurn skip; run skip)

    fun runAlt  () = run (fn _ => false)
    fun runOnly () = run (fn S.P1 => false | S.P2 => true)

    val arg0 = CommandLine.name()
    val _ = case CommandLine.arguments()
              of ["only"] => runOnly ()
               | ["alt"] => runAlt ()
                  | _ => app print ["Usage: ", arg0, " only\n",
                                    "              (one player)\n",
                                    "       ", arg0, " alt\n",
                                    "              (two players)\n"]
end
