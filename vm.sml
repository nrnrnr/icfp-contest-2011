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
  datatype 'a v = N of int
                | F of unit v -> unit v
  val cast  : 'a v -> 'b v  (* change phantom type *)
  val apply : ('a -> 'b) v * 'a v -> 'b v
end

structure Value :> VALUE = struct
  exception Error of string
  datatype 'a v = N of int
                | F of unit v -> unit v
  fun cast (N n) = N n
    | cast (F f) = F f
  fun apply (F f, v) = cast (f (cast v))
    | apply (N n, _) = raise Error "applied integer"
end

    
    