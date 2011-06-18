
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
       | :@: (t1, t2) => :@: (castTerm t1, castTerm t2)

end

structure TermCard : CARD = struct
  type slot = int
  type slot' = int
  type 'a card = 'a Term.t
  type u = unit
  datatype unitype = U of unitype
  val untyped = Term.cast
  
  open Term
end

functor TranslateTermFn(Card2 : CARD) : CARD_TRANSLATE = struct
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

functor Embedding (val error : string -> 'a) = struct
  structure T = Term
  val show = ShowTerm.show
  fun err ss = error (String.concat ss)
  fun toInt (T.N n) = n
    | toInt t = err ["tried to project ", show t, " as integer"]

  type 'a pair = { embed : 'a -> 'a Term.t, project : 'a Term.t -> 'a }

  val castpair : 'a pair -> { embed : 'a -> 'b Term.t, project : 'b Term.t -> 'a }
    = fn {embed = e, project = p} => { embed   = fn a => T.castTerm (e a)
                                     , project = fn t => p (T.castTerm t)
                                     } 

  val int : int pair = { embed = T.N, project = toInt }
(*
    fun --> apply (arg : 'a pair, res : 'b pair) : ('a -> 'b) pair = 
      { embed = (fn f => #embed res o f o #project arg)
                                   : ('a -> 'b) -> ('a -> 'b) Term.t
      , project = (fn v => #project res o apply v o #embed arg) :
              ('a -> 'b) Term.t -> 'a -> 'b
       }
*)
(*
  val a : 'a v pre_pair = { embed = cast, project = cast }
    val id : unit pair = fn clock =>
        { embed   = fn () => F (Clock.tick clock (fn x => x))
        , project = fn _ => ()
        }
*)
end



functor TermCombinatorsFn (val clock : Clock.t
                           structure VM : CARD where type 'a card = 'a
                                                 and type slot = int
                                                 and type slot' = int
                                                 and type u = unit
                          ) : COMBINATORS = struct
  structure Card = TermCard
  open Term

  infix 3 @@
  infix 4 :@:
  exception Error of string
  val show = ShowTerm.show

  structure E = Embedding (fun error s = raise Error s)

  fun embed {embed, project} = embed
  fun project {embed, project} = project

  val int = { embed   = fn x => #embed (E.castpair E.int) x
            , project = fn x => #project (E.castpair E.int) x
            }
  fun --> (arg, res) = { embed = fn f => embed res o f o project arg
                       , project = fn v => raise Error "higher-order function"
                       }
  val a = { embed = castTerm, project = fn x => x }
  val u = { embed = fn () => C I, project = fn _ => raise Error "unit argument" }

  infixr 2 -->
  infixr 0 $
  fun f $ x = f x

  val cast = castTerm
  fun card zero = N 0
    | card c = C c

  val e = embed

    (* invariant: results are always in normal form *)
  fun (C I : 'a t) @@ (x : 'a t) : 'a t = x
    | (C K :@: x) @@ y       = cast x
    | (C S :@: f :@: g) @@ x = f @@ x @@ (g @@ x)
    | (C put)  @@ _          = cast (C I)
    | (C succ) @@ n          = cast $ e (int --> int)  VM.succ n
    | (C dbl)  @@ n          = cast $ e (int --> int)  VM.dbl  n
    | (C get)  @@ i          = cast $ e (int --> a)    VM.get  i
    | (C inc)  @@ i          = cast $ e (int --> u)    VM.inc  i
    | (C dec)  @@ i          = cast $ e (int --> u)    VM.dec  i
    | (C attack :@: i :@: j) 
                        @@ n = cast $ e (int --> int --> int --> u) VM.attack i j n
    | (C help   :@: i :@: j) 
                        @@ n = cast $ e (int --> int --> int --> u) VM.help   i j n
    | (C copy) @@ i          = cast $ e (int --> u)       VM.copy i
    | (C revive) @@ i        = cast $ e (int --> u)       VM.revive i
    | (C zombie :@: i) @@ x  = cast $ e (int --> a --> u) VM.zombie i x
    | (C zero) @@ _          = raise Error "applied zero"
    | f @@ x                 = f :@: x


  val op @@ = fn (f, x) => castTerm (Clock.tick clock op @@ (castTerm f, x))
  
end
