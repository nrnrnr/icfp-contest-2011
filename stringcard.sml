(*functor translateStringCar(card2 : string) : CARD = struct
  *)



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

  exception Failed of string
  fun translate t =
    case t
      of "I" => C2.cast C2.I
       | "S" => C2.cast C2.S
       | "K" => C2.cast C2.K
       | "put" => C2.cast C2.put
       | "zero" => C2.cast C2.zero
       | "succ" => C2.cast C2.succ
       | "dbl" => C2.cast C2.dbl
       | "get" => C2.cast C2.get
       | "copy" => C2.cast C2.copy
       | "inc" => C2.cast C2.inc
       | "dec" => C2.cast C2.dec
       | "attack" => C2.cast C2.attack
       | "help" => C2.cast C2.help
       | "revive" => C2.cast C2.revive
       | "zombie" => C2.cast C2.zombie
       | _ => raise Failed
end

