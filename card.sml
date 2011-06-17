signature CARD = sig
  type slot
  type slot'
  type 'a card

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

  val get  : (slot -> 'a) card  (* load from slot or fail *)
  val copy : (slot -> 'a) card  (* load from *opponent's* slot or fail *)

  (* vitality *)
  val inc : (slot  -> ('a -> 'a)) card   (* increment our vitality *)
  val dec : (slot' -> ('a -> 'a)) card   (* decrement their vitality *)
  val attack : (slot -> slot' -> int -> ('a -> 'a)) card (* decrease everyone's vitality *)
  val help   : (slot -> slot  -> int -> ('a -> 'a)) card (* transfer vitality internally *)
  val revive : (slot -> ('a -> 'a)) card (* force vitality to 1 *)
  val zombie : (slot' -> 'a -> ('b -> 'b)) card (* create zombie in opponent *)
end

(* we'll need several representations *)
signature CARD_TRANSLATE = sig
  structure C1 : CARD
  structure C2 : CARD
  val translate : 'a C1.card -> 'a C1.card
end
