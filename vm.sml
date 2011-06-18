signature VALUE = sig
  exception Error of string
  datatype 'a v = N of int
                | F of unit v -> unit v
  val cast  : 'a v -> 'b v  (* change phantom type *)
  val apply : ('a -> 'b) v * 'a v -> 'b v

  val toInt : int v -> int
  val toFun : ('a -> 'b) v -> 'a v -> 'b v
end

structure Value :> VALUE = struct
  exception Error of string
  datatype 'a v = N of int
                | F of unit v -> unit v
  fun cast (N n) = N n
    | cast (F f) = F f
  fun apply (F f, v) = cast (f (cast v))
    | apply (N n, _) = raise Error "applied integer"


  fun toInt (N n) = n
    | toInt (F _) = raise Error "cast function to integer"

  fun toFun (N n) = raise Error "cast integer to function"
    | toFun (F f) = fn v => cast (f (cast v))
end




functor Run(structure Value : VALUE
            type vitality = int
            val automatic : bool ref (* are we in the auto phase? *)
            val proponent : (vitality * 'a Value.v) array
            val opponent  : (vitality * 'a Value.v) array
            ) : sig
                  include CARD where type 'a card = 'a Value.v
                  val cardToSlot : ('a -> 'b) card -> int -> unit
                  val slotToCard : int -> ('a -> 'b) card -> unit
                end
=
struct
  type 'a card = 'a Value.v
  open Value

  type 'a pair = { embed : 'a -> 'a v, project : 'a v -> 'a }
  structure B = struct (* bijection *)
    val int : int pair = { embed = N, project = toInt }
    fun --> (arg : 'a pair, res : 'b pair) : ('a -> 'b) pair =
        { embed   =
           (fn f => cast (F (fn v => cast (#embed res (f (#project arg (cast v)))))))
                      : ('a -> 'b) -> ('a -> 'b) v
        , project = (fn v => fn a => #project res (toFun v (cast (#embed arg a)))) :
              ('a -> 'b) v -> 'a -> 'b
        }
    val a : 'a v pair = { embed = cast, project = cast }
    val id : unit pair = { embed   = fn () => F (fn x => x)
                         , project = fn _ => ()
                         }
  end
  val --> = B.-->
  infixr 2 -->                          
  val (a, b, c) = (B.a, B.a, B.a)



  type slot  = int  (* reference to slot i *)
  type slot' = int  (* reference to slot 255-i *)

    fun asFun { embed, project } f =  (* deal with stupid value restriction *)
        case embed f
          of F f => f
           | N _ => let exception Can'tHappen in raise Can'tHappen end


  (* combinators *)


    fun I x = x
    val I = F (fn v => asFun (a --> a) I v)
    fun S f g x = f x (g x)
    val S = F (fn v => asFun ((a --> b --> c) --> (a --> b) --> (a --> c)) S v)
    fun K x y = x
    val K = F (fn v => asFun (a --> b --> a) K v)
    fun put x y = y
    val put = F (fn v => asFun (a --> b --> a) put v)

  (* numbers *)

  fun pinhi n = if n < 65535 then 65535 else n
  fun pinlo n = if n < 0 then 0 else n
  val pin = pinhi o pinlo

  val zero = #embed B.int 0
  val succ = #embed (B.int --> B.int) (fn n => pin (n + 1))
  val dbl  = #embed (B.int --> B.int) (fn n => pin (n + n))


  (* slots *)

  fun field (vitality, field) = field
  fun vitality (vitality, field) = vitality

  fun our   slot = Array.sub (proponent, slot)
  fun their slot = Array.sub (opponent,  slot)
  fun their' slot' = their (255 - slot')

  infix 1 <-: :-> <=: :=>

  fun slot <-: v = Array.update (proponent, slot, (v, field (our   slot)))
  fun v :-> slot = Array.update (opponent,  slot, (v, field (their slot)))

  fun slot <=: x = Array.update (proponent, slot, (vitality (our   slot), x))

  fun get  slot = field (our   slot)
  fun copy slot = field (their slot)
  val get  = F (fn v => asFun (B.int --> a) get v)
  val copy = F (fn v => asFun (B.int --> a) copy v)

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

  val inc = F (fn v => asFun (B.int --> B.id) inc v)
  val dec = F (fn v => asFun (B.int --> B.id) dec v)

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

  val attack = F (fn v => asFun (B.int --> B.int --> B.int --> B.id) attack v)
  val help   = F (fn v => asFun (B.int --> B.int --> B.int --> B.id) help   v)
  val revive = F (fn v => asFun (B.int --> B.id) revive v)
  val zombie = F (fn v => asFun (B.int --> B.a --> B.id) zombie v)


  fun toFun' f = cast o toFun f o cast

  fun liveField x = if vitality x > 0 then field x
                    else raise Error "pulled a field from a dead slot"

  fun cardToSlot card slot =
        slot <=: toFun' card (liveField (our slot)) handle _ => slot <=: I
  fun slotToCard slot card =
        slot <=: toFun' (liveField (our slot)) card handle _ => slot <=: I
end
