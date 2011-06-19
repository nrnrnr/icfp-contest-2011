structure Unitype : sig type t end = struct
  datatype t = U of t  (* type parameter given to untyped cards/terms/values *)
end

signature CARD = sig
  type slot
  type slot'
  type 'a card

  val untyped : 'a card -> Unitype.t card
  val cast : 'a card -> 'b card (* can't be helped *)

  (* combinators *)

  val I    : ('a -> 'a) card
  val S    : (('a -> 'b -> 'c) -> ('a -> 'b) -> ('a -> 'c)) card
  val K    : ('a -> 'b -> 'a) card
  val put  : ('a -> 'b -> 'b) card

  (* numbers *)

  val zero : int card
  val succ : (int -> int) card
  val dbl  : (int -> int) card

  (* slots *)

  type field
  val get  : (slot -> field) card  (* load from slot or fail *)
  val copy : (slot -> field) card  (* load from *opponent's* slot or fail *)

  (* vitality *)

  type u (* unit type, for things that return the identity function *)

  val inc : (slot  -> u) card   (* increment our vitality *)
  val dec : (slot' -> u) card   (* decrement their vitality *)
  val attack : (slot -> slot' -> int -> u) card (* decrease everyone's vitality *)
  val help   : (slot -> slot  -> int -> u) card (* transfer vitality internally *)
  val revive : (slot -> u) card (* force vitality to 1 *)
  val zombie : (slot' -> field -> u) card (* create zombie in opponent *)
end

signature COMBINATORS = sig
  structure Card : CARD
  type 'a t
  val card : 'a Card.card -> 'a t
  val @@ : ('a -> 'b) t * 'a t -> 'b t
end

(* we'll need several representations *)
signature CARD_TRANSLATE = sig
  structure C1 : CARD
  structure C2 : CARD
  exception Failed of string
  val translate : 'a C1.card -> 'a C2.card
  (* Could raise Failed *)
end
