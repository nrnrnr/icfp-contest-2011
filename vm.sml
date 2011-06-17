signature VM = sig
  type slot  = int  (* reference to slot i *)
  type slot' = int  (* reference to slot 255-i *)
  type 'a v   (* a value or function *)

  val @@ : ('a -> 'b) v * 'a v -> 'b v   (* apply *)

  (* combinators *)

  val I    : ('a -> 'a) v
  val S    : (('a -> 'b -> 'c) -> ('a -> 'b) -> ('a -> 'c)) v
  val K    : ('a -> 'b -> 'a) v
  val put  : ('a -> 'b -> 'b) v

  (* numbers *)

  val zero : int v
  val succ : (int -> int) v
  val dbl  : (int -> int) v

  (* slots *)

  val get  : (slot -> 'a) v  (* load from slot or fail *)
  val copy : (slot -> 'a) v  (* load from *opponent's* slot or fail *)

  (* vitality *)
  val inc : (slot  -> ('a -> 'a)) v   (* increment our vitality *)
  val dec : (slot' -> ('a -> 'a)) v   (* decrement their vitality *)
  val attack : (slot -> slot' -> int -> ('a -> 'a)) v (* decrease everyone's vitality *)
  val help   : (slot -> slot  -> int -> ('a -> 'a)) v (* transfer vitality internally *)
  val revive : (slot -> ('a -> 'a)) v (* force vitality to 1 *)
  val zombie : (slot' -> 'a -> ('b -> 'b)) v (* create zombie in opponent *)
end

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
            val proponent : (vitality * 'a Value.v) array
            val opponent  : (vitality * 'a Value.v) array
            ) : VM where type 'a v = 'a Value.v = struct
  open Value
  val @@ = apply
  infix 3 @@

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

  fun pin n = if n < 65535 then 65535 else n

  val zero = #embed B.int 0
  val succ = #embed (B.int --> B.int) (fn n => pin (n + 1))
  val dbl  = #embed (B.int --> B.int) (fn n => pin (n + n))


  (* slots *)

  fun field (vitality, field) = field

  fun get  slot = field (Array.sub (proponent, slot))
  fun copy slot = field (Array.sub (opponent,  slot))
  val get  = F (fn v => asFun (B.int --> a) get v)
  val copy = F (fn v => asFun (B.int --> a) copy v)

  (* vitality *)
  fun undefined x = undefined x

  val inc = F undefined
  val dec = F undefined
  val attack = F undefined
  val help   = F undefined
  val revive = F undefined
  val zombie = F undefined



end
