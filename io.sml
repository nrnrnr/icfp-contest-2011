signature MOVE_IO = sig
  type player = string
  structure Move : MOVE where type 'a Card.card = string
  val printMove : player -> Move.t -> unit
  val readMove  : player -> Move.t
end

functor MoveIOFn(structure Move : MOVE where type 'a Card.card = string
                 val prompt : bool) : MOVE_IO where type Move.t = Move.t
 =
struct
  structure Move = Move
  type player = string
  structure M = Move
  local 
    fun prompt s =
      ( app print [s, "\n"]
      ; TextIO.flushOut TextIO.stdOut
      ; case TextIO.inputLine TextIO.stdIn
          of SOME s => s
           | NONE => prompt s
      )
    fun noprompt (_ : string) =
      case TextIO.inputLine TextIO.stdIn
        of SOME s => s
         | NONE => let exception EOF in raise EOF end
    fun strip_newline s =
      let val n = size s
          val last = if n > 0 then String.sub(s, n-1) else #"x"
      in  if last = #"\n" then
            String.substring(s, 0, n-1)
          else
            s
      end
  in
    val (p, np) = (strip_newline o prompt, strip_newline o noprompt)
  end
  val prompter = if prompt then p else np
  
  fun printMove player (M.CardToSlot (c, s)) =
        app print [player, " applied card ", c, " to slot ", Int.toString s, "\n"]
    | printMove player (M.SlotToCard (s, c)) =
        app print [player, " applied slot ", Int.toString s, " to card ", c, "\n"]
  fun quietMove player (M.CardToSlot (c, s)) =
        app print ["1\n", c, "\n", Int.toString s, "\n"]
    | quietMove player (M.SlotToCard (s, c)) =
        app print ["2\n", Int.toString s, "\n", c, "\n"]

  val printMove = if prompt then printMove else quietMove

  fun readMove player =
    case prompter "(1) apply card to slot, or (2) apply slot to card?"
      of "1" => readCardToSlot ()
       | "2" => readSlotToCard ()
       | _   => readMove player
  and readCardToSlot () =
        let val card = readCard ()
            val slot = readSlot ()
        in  M.CardToSlot (card, slot)
        end
  and readSlotToCard () =
        let val slot = readSlot ()
            val card = readCard ()
        in  M.SlotToCard (slot, card)
        end
  and readCard () = prompter "card name?"
  and readSlot () =
        case Int.fromString (prompter "slot no?")
          of SOME n => n
           | NONE => readSlot ()
         

end
