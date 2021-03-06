
(* to apply a function a[i] to a[0]
     S (K a[i]) get zero
*)


structure Term = struct
  datatype 'a card
    = I | S | K | put
    | zero | succ | dbl
    | get | copy
    | inc | dec | attack | help | revive | zombie

  datatype 'a t
    = C  of 'a card
    | N  of int
    | :@: of 'a t * 'a t  (* general application *)

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
       | :@: (t1, t2) => :@: (castTerm t1, castTerm t2)

end

structure TermCard : CARD = struct
  type slot = int
  type slot' = int
  type 'a card = 'a Term.t
  type u = unit
  type field = Unitype.t Term.t
  val untyped = Term.cast
  
  open Term
end

functor TranslateTermFn(Card2 : CARD) : CARD_TRANSLATE = struct
  structure C1 = TermCard
  structure C2 = Card2
  exception Failed of string
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


structure ShowTerm : sig
    val show : 'a Term.t -> string
 end
=
struct
  structure Tx = TranslateTermFn(StringCard)
  val showCard : 'a Term.card -> string = Tx.translate
  structure T = Term
  fun bracket s = "(" ^ s ^ ")"
  fun nobracket s = s

  fun show' br t =
    case t
      of T.C c => showCard c
       | T.N n => Int.toString n
       | Term.:@: (t1, t2) => br (show' nobracket t1 ^ " " ^ show' bracket t2)

  fun show t = show' nobracket t
end

functor TermCombinatorsFn (val clock : Clock.t
                           structure VM : CARD where type 'a card = 'a
                                                 and type slot = int
                                                 and type slot' = int
                                                 and type u = unit
                                                 and type field = Unitype.t Term.t
                          ) : COMBINATORS =
struct
  structure Card = TermCard
  val clock = clock
  open Term

  infix 3 @@ @-@
  infix 4 :@:
  exception Error of string
  val show = ShowTerm.show

  fun err ss = raise Error (String.concat ss)

  (**** embedding and projection ****)
  fun embed {embed, project} = embed
  fun project {embed, project} = project

  val int = { embed   = N
            , project = fn (N n) => n | t => err ["projected ", show t, " to integer"]
            }
  fun --> (arg, res) = { embed = fn f => embed res o f o project arg
                       , project = fn v => raise Error "higher-order function"
                       }
  infixr 2 -->
  val f = { embed = castTerm, project = castTerm }  (* a field *)
  val u = { embed = fn () => C I, project = fn _ => raise Error "unit argument" }
  (************************************)

  infixr 0 $
  fun f $ x = f x

  val cast = castTerm
  fun card zero = N 0
    | card c = C c

  val e = embed

    (* invariant: results are always in normal form *)
  fun (C I : 'a t) @@ (x : 'a t) : 'a t = x
    | (C K :@: x) @@ y       = cast x
    | (C S :@: f :@: g) @@ x = f @-@ x @-@ (g @-@ x)
    | (C put)  @@ _          = cast (C I)
    | (C succ) @@ n          = cast $ e (int --> int)  VM.succ n
    | (C dbl)  @@ n          = cast $ e (int --> int)  VM.dbl  n
    | (C get)  @@ i          = cast $ e (int --> f)    VM.get  i
    | (C copy) @@ i          = cast $ e (int --> f)    VM.copy i
    | (C inc)  @@ i          = cast $ e (int --> u)    VM.inc  i
    | (C dec)  @@ i          = cast $ e (int --> u)    VM.dec  i
    | (C attack :@: i :@: j) 
                        @@ n = cast $ e (int --> int --> int --> u) VM.attack i j n
    | (C help   :@: i :@: j) 
                        @@ n = cast $ e (int --> int --> int --> u) VM.help   i j n
    | (C revive) @@ i        = cast $ e (int --> u)       VM.revive i
    | (C zombie :@: i) @@ x  = cast $ e (int --> f --> u) VM.zombie i x
    | (C zero) @@ _          = raise Error "applied zero"
    | (f as C K) @@ x        = f :@: x
    | (f as C S) @@ x        = f :@: x
    | (f as (C S :@: _)) @@ x      = f :@: x
    | (f as C attack) @@ x         = f :@: x
    | (f as (C attack :@: _)) @@ x = f :@: x
    | (f as C help) @@ x           = f :@: x
    | (f as (C help :@: _)) @@ x   = f :@: x
    | (f as C zombie) @@ x         = f :@: x
    | (f as _ :@: _) @@ x          = f :@: x
    | (N n) @@ t = err ["applied integer ", Int.toString n, " to " , show t]
  and f @-@ x = Clock.tick clock op @@ (f, x)

  fun f @@ x = castTerm (f @-@ castTerm x)
  
end
