signature AUTOPLAYER = sig
  structure Sim : SIMULATOR
  structure Move : MOVE sharing type t = Sim.Move.t
  type state
  val start : Sim.player -> state  (* tell the AI which player he is *)
  val opponentMoved : Move.t * state -> state
  val youMoved      : Move.t * state -> state
  val abort : state -> Move.t list -> bool
     (* given current state, should we abort the moves already planned? *)
  val whatNext : state -> Move.t list  (* what to do next? *)
end

functor RunAIFn(structure AI : AUTOPLAYER
                structure Move : MOVE where type 'a Card.card = string
                structure Tx : CARD_TRANSLATE
                      where type 'a C1.card = 'a Move.Card.card
                        and type 'a C2.card = 'a TermCard.card
                val printMove : Sim.player -> Move.t -> unit
                val getMove   : Sim.player -> Move.t) = 
struct
  structure S = AI.Sim
  structure TxMove = TxMoveFn(structure M1 = Move
                              structure M2 = S.Move
                              structure Tx = Tx)

  val weAre = case CommandLine.arguments()
                of ["0"] => S.P1
                 | ["1"] => S.P2
  val opponent = S.otherplayer weAre

  val start = AI.start weAre

  fun opponentMoves state =
    let val move = getMove opponent
        val _ = S.step (TxMove.translate move)
        val state = opponentMoved (move, state)
    in  state
    end

  val state = case weAre of S.P2 => opponentMoves state | S.P1 => state
  fun run instructions state =
    if S.game_over () then
      ()
    else if null instructions orelse AI.abort state instructions then
      run (AI.whatNext state) state
    else
      case instructions
        of [] => run (AI.whatNext state) state (* should never happen *)
         | m :: ms =>
            let val _ = S.step m
                val state = AI.youMoved
      