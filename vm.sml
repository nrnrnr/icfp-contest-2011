signature VM = sig
  include CARD where type 'a card = 'a
                 and type slot = int
                 and type slot' = int
                 and type u = unit
  exception Error of string
  val clock : Clock.t
  val <=: : slot * field -> unit
  val ourLiveField : slot -> field
end

signature VM_PAIR = sig
  type vitality = int
  structure Player1 : VM
  structure Player2 : VM
  sharing type Player1.field = Player2.field
  val clock : Clock.t
  val automatic : bool ref
  val slots1 : (vitality * Player1.field) array
  val slots2 : (vitality * Player2.field) array
end

functor VMFn(type field
             type vitality = int
             val this_clock : Clock.t
             val automatic : bool ref (* are we in the auto phase? *)
             val proponent : (vitality * field) array
             val opponent  : (vitality * field) array
            ) :> VM where type field = field 
 =
struct
  type 'a card = 'a
  val clock = this_clock
  fun impossible s = let exception ThisCan'tHappen of string
                     in  raise ThisCan'tHappen s
                     end

  exception Error of string
  fun untyped t = impossible "untyped VM instruction"
  fun cast f = impossible "cast VM instruction"
  type u = unit
  type slot  = int  (* reference to slot i *)
  type slot' = int  (* reference to slot 255-i *)
  type field = field

    fun I x = x
    fun S f g x = f x (g x)
    fun K x y = x
    fun put x y = y

  (* numbers *)

  fun pinhi n = if n > 65535 then 65535 else n
  fun pinlo n = if n < 0 then 0 else n
  val pin = pinhi o pinlo

  val zero = 0
  fun succ n = pin (n + 1)
  fun dbl  n = pin (n + n)

  (* slots *)

  fun field (vitality, field) = field
  fun vitality (vitality, field) = vitality

  fun our   slot = Array.sub (proponent, slot)
  fun their slot = Array.sub (opponent,  slot)
  fun their' slot' = their (255 - slot')

  fun liveField x = if vitality x > 0 then field x
                    else raise Error "pulled a field from a dead slot"

  fun ourLiveField slot = liveField (our slot)


  infix 1 <-: :-> <=: :=>

  fun slot <-: v = Array.update (proponent, slot, (v, field (our   slot)))
  fun v :-> slot = Array.update (opponent,  slot, (v, field (their slot)))

  fun slot <=: x = Array.update (proponent, slot, (vitality (our   slot), x))

  fun get  slot = field (our   slot)
  fun copy slot = field (their slot)

  (* vitality *)
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
end


functor VMPairFn(type field
                 val initialField : field) : VM_PAIR where type Player1.field = field
  =
struct
  type vitality = int
  val initial_vitality = 10000
  val clock = Clock.mk ()
  val slot = (initial_vitality, initialField)
  val slots1 = Array.tabulate (256, fn _ => slot)
  val slots2 = Array.tabulate (256, fn _ => slot)
  val automatic = ref false

  structure Player1 = VMFn(type vitality  = int
                           type field     = field
                           val this_clock = clock
                           val automatic  = automatic
                           val proponent  = slots1
                           val opponent   = slots2)

  structure Player2 = VMFn(type vitality  = int
                           type field     = field
                           val this_clock = clock
                           val automatic  = automatic
                           val proponent  = slots2
                           val opponent   = slots1)
end

