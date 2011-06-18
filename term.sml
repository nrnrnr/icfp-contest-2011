structure Term = struct
  datatype 'a card
    = I | S | K | put
    | zero | succ | dbl
    | get | copy
    | inc | dec | attack | help | revive | zombie

  datatype 'a t
    = C  of 'a card
    | P1 of 'a partial1  (* partial applications of a term *)
    | P2 of 'a partial2
    | N  of int
  withtype 'a partial1 = 'a card * 'a t
      and  'a partial2 = 'a partial1 * 'a t

  fun cast c =
    case c
      of I => I | S => S | K => K | put => put
       | zero => zero | succ => succ | dbl => dbl
       | get => get | copy => copy
       | inc => inc | dec => dec | attack => attack
       | help => help | revive => revive | zombie => zombie

  fun castTerm t =
    case t
      of C c => C (cast c)
       | N n => N n
       | P1 p => P1 (cast1 p)
       | P2 p => P2 (cast2 p)
  and cast1 (c, t) = (cast c, castTerm t)
  and cast2 (p, t) = (cast1 p, castTerm t)

end

structure TermCard : CARD = struct
  type slot = int
  type slot' = int
  type 'a card = 'a Term.t
  datatype unitype = U of unitype
  val untyped = Term.cast
  
  open Term
end

functor TranslateTerm(Card2 : CARD) : CARD_TRANSLATE = struct
  structure C1 = TermCard
  structure C2 = Card2
  fun translate t =
    case t
      of Term.I => C2.cast C2.I
       | Term.S => C2.cast C2.S
       | Term.K => C2.cast C2.K
       | Term.put => C2.cast C2.put
       | Term.zero => C2.cast C2.zero
       | Term.succ => C2.cast C2.succ
       | Term.dbl => C2.cast C2.dbl
       | Term.get => C2.cast C2.get
       | Term.copy => C2.cast C2.copy
       | Term.inc => C2.cast C2.inc
       | Term.dec => C2.cast C2.dec
       | Term.attack => C2.cast C2.attack
       | Term.help => C2.cast C2.help
       | Term.revive => C2.cast C2.revive
       | Term.zombie => C2.cast C2.zombie
end


functor ShowTerm(val showCard : 'a Term.t -> string) = struct
  structure T = Term
  fun bracket s = "(" ^ s ^ ")"
  fun nobracket s = s

(*
  fun show br t =
    case t
      of T.N n => Int.toString n
       | Term.P1 (f, arg) => 
    | 
*)

(* to apply a function a[i] to a[0]
     S (K a[i]) get zero
*)

end

