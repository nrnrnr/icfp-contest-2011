(*
functor VMFn(type field
             type vitality
             val this_clock : Clock.t
             val automatic : bool ref (* are we in the auto phase? *)
             val proponent : (vitality * field) array
             val opponent  : (vitality * field) array
            ) :> CARD where type 'a card = 'a
                        and type slot = int
                        and type slot' = int
                        and type u = unit
 =
struct
end
*)

structure ES = struct val x = 99 end

(*

functor RunFn(structure Value : VALUE
            type vitality = int
            val this_clock : Clock.t
            val automatic : bool ref (* are we in the auto phase? *)
            val proponent : (vitality * unit Value.v) array
            val opponent  : (vitality * unit Value.v) array
            ) : sig
                  include CARD where type 'a card = 'a Value.v
                  val cardToSlot : ('a -> 'b) card -> int -> unit
                  val slotToCard : int -> ('a -> 'b) card -> unit
                end
=
struct
  type 'a card = 'a Value.v
  open Value
  datatype unitype = U of unitype
  val untyped = cast
  type u = unit

  type 'a pre_pair = { embed : 'a -> 'a v, project : 'a v -> 'a }
  type 'a pair = Clock.t -> 'a pre_pair
  structure B = struct (* bijection *)
    val int : int pair = fn _ => { embed = N, project = toInt }
    fun --> (arg : 'a pair, res : 'b pair) : ('a -> 'b) pair = fn clock => 
      let val (arg, res) = (arg clock, res clock)
          val c = Clock.tick clock
      in  { embed   =
             (fn f => cast (F (cast o #embed res o c f o #project arg o cast)))
                      : ('a -> 'b) -> ('a -> 'b) v
          , project = (fn v => #project res o toFun v o cast o #embed arg) :
              ('a -> 'b) v -> 'a -> 'b
          }
      end
    val a : 'a v pre_pair = { embed = cast, project = cast }
    val id : unit pair = fn clock =>
        { embed   = fn () => F (Clock.tick clock (fn x => x))
        , project = fn _ => ()
        }
  end
  fun --> (arg, res) = B.--> (fn _ => arg, fn _ => res) this_clock
  infixr 2 -->                          
  val (a, b, c) = (B.a, B.a, B.a)
  val Bid = B.id this_clock
  val Bint = B.int this_clock


  type slot  = int  (* reference to slot i *)
  type slot' = int  (* reference to slot 255-i *)

    fun asFun { embed, project } f =  (* deal with stupid value restriction *)
        case embed f
          of F f => f
           | N _ => let exception Can'tHappen in raise Can'tHappen end


  (* combinators *)


    fun I x = x
    fun S f g x = f x (g x)
    fun K x y = x
    fun put x y = y

    val I = F (fn v => asFun (a --> a) I v)
    val S = F (fn v => asFun ((a --> b --> c) --> (a --> b) --> (a --> c)) S v)
    val K = F (fn v => asFun (a --> b --> a) K v)
    val put = F (fn v => asFun (a --> b --> a) put v)

  (* numbers *)

  fun pinhi n = if n < 65535 then 65535 else n
  fun pinlo n = if n < 0 then 0 else n
  val pin = pinhi o pinlo

  val zero = #embed Bint 0
  val succ = #embed (Bint --> Bint) (fn n => pin (n + 1))
  val dbl  = #embed (Bint --> Bint) (fn n => pin (n + n))


  (* slots *)

  fun field (vitality, field) = cast field
  fun vitality (vitality, field) = vitality

  fun our   slot = Array.sub (proponent, slot)
  fun their slot = Array.sub (opponent,  slot)
  fun their' slot' = their (255 - slot')

  infix 1 <-: :-> <=: :=>

  fun slot <-: v = Array.update (proponent, slot, (v, field (our   slot)))
  fun v :-> slot = Array.update (opponent,  slot, (v, field (their slot)))

  fun slot <=: x = Array.update (proponent, slot, (vitality (our   slot), cast x))

  fun get  slot = field (our   slot)
  fun copy slot = field (their slot)

  val get  = F (fn v => asFun (Bint --> a) get v)
  val copy = F (fn v => asFun (Bint --> a) copy v)

  (* vitality *)
  fun undefined x = undefined x

  fun increase slot v = if v > 0 andalso v < 65535 then slot <-: v + 1 else ()
  fun decrease slot v = if v > 0                   then slot <-: v - 1 else ()

  fun inc slot =
    let val v = vitality (our slot)
    in  if !automatic then
          decrease slot v
        else
          increase slot v
    end

  fun dec slot' =
    let val slot = 255 - slot'
        val v = vitality (their slot)
    in  if !automatic then increase slot v else decrease slot v
    end

  val inc = F (fn v => asFun (Bint --> Bid) inc v)
  val dec = F (fn v => asFun (Bint --> Bid) dec v)

  fun take_n_from_our n slot = 
    let val v = vitality (our slot)
    in  if n > v then
          raise Error "attacked without enough vitality"
        else
          slot <-: v - n
    end

  fun attack slot slot' n =
    let val _ = take_n_from_our n slot
        val delta = (9 * n) div 10
        val w = vitality (their' slot')
        val new_w = pin (if !automatic then if w > 0 then w + delta else w
                         else w - delta)
    in  if w > 0 then new_w :-> 255 - slot' else ()
    end

  fun help from to n =
    let val _ = take_n_from_our n from
        val delta = (11 * n) div 10
        val delta = if !automatic then ~delta else delta
        val w = vitality (our to)
    in  if w > 0 then to <-: pin (w + delta) else ()
    end
            
  fun revive slot = if vitality (our slot) <= 0 then slot <-: 1 else ()
  fun zombie slot' x =
    let val v = vitality (their' slot')
    in  if v > 0 then
          raise Error "tried to zombie a live slot"
        else
          Array.update (opponent, 255-slot', (~1, x))
    end

  val attack = F (fn v => asFun (Bint --> Bint --> Bint --> Bid) attack v)
  val help   = F (fn v => asFun (Bint --> Bint --> Bint --> Bid) help   v)
  val revive = F (fn v => asFun (Bint --> Bid) revive v)
  val zombie = F (fn v => asFun (Bint --> a --> Bid) zombie v)


  fun toFun' f = cast o toFun f o cast

  fun liveField x = if vitality x > 0 then field x
                    else raise Error "pulled a field from a dead slot"

  fun cardToSlot card slot =
        slot <=: toFun' card (liveField (our slot)) handle _ => slot <=: I
  fun slotToCard slot card =
        slot <=: toFun' (liveField (our slot)) card handle _ => slot <=: I

  fun with_fresh_clock f x y = (Clock.reset this_clock; f x y)
  val cardToSlot = fn x => with_fresh_clock cardToSlot x
  val slotToCard = fn y => with_fresh_clock slotToCard y
end


signature MOVE = sig
  structure Card : CARD
  type card = Card.unitype Card.card
  datatype t = CardToSlot of card * int
             | SlotToCard of int * card
end

functor Move (Card : CARD) : MOVE = struct
  structure Card = Card
  type card = Card.unitype Card.card
  datatype t = CardToSlot of card * int
             | SlotToCard of int * card
end

functor RunTest (structure Move : MOVE
                 structure Tx : CARD_TRANSLATE
                     where type 'a C1.card = 'a Move.Card.card
                       and type 'a C2.card = 'a Value.v
                 val player1 : Move.t option -> Move.t
                 val player2 : Move.t option -> Move.t) = 
struct
  type vitality = int
  val initial_vitality = 10000
  val this_clock = Clock.mk ()
  val I : unit Value.v = Value.F (Clock.tick this_clock (fn x => x))
  val slot = (initial_vitality, I)
  val slots1 = Array.tabulate (256, fn _ => slot)
  val slots2 = Array.tabulate (256, fn _ => slot)
  val automatic = ref false

  structure Run1 = RunFn
      (structure Value = Value
       type vitality = int
       val this_clock = this_clock
       val automatic = automatic
       val proponent = slots1
       val opponent = slots2)

  structure Run2 = RunFn
      (structure Value = Value
       type vitality = int
       val this_clock = this_clock
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
