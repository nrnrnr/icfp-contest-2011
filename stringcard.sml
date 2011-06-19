structure StringCard : CARD = struct
  type 'a card = string
  type slot = int
  type slot' = int
  type field = unit

  fun untyped c = c
  fun cast c = c

  (* combinators *)

  val I   = "I"
  val S   = "S"
  val K   = "K"
  val put = "put"

  (* numbers *)

  val zero = "zero"
  val succ = "succ"
  val dbl  = "dbl"

  (* slots *)

  val get  = "get"
  val copy = "copy"

  (* vitality *)

  type u = unit

  val inc    = "inc"
  val dec    = "dec"
  val attack = "attack"
  val help   = "help"
  val revive = "revive"
  val zombie = "zombie"
end

