signature MOVE = sig
  structure Card : CARD
  type card = Unitype.t Card.card
  datatype t = CardToSlot of card * int
             | SlotToCard of int * card
end

functor MoveFn (Card : CARD) : MOVE = struct
  structure Card = Card
  type card = Unitype.t Card.card
  datatype t = CardToSlot of card * int
             | SlotToCard of int * card
end

signature MOVE_TRANSLATE = sig
  structure M1 : MOVE
  structure M2 : MOVE
  val translate : M1.t -> M2.t
end

functor TxMoveFn(structure M1 : MOVE
                 structure M2 : MOVE
                 structure Tx : CARD_TRANSLATE
                                  where type 'a C1.card = 'a M1.Card.card
                                    and type 'a C2.card = 'a M2.Card.card
                ) : MOVE_TRANSLATE = struct
  structure M1 = M1
  structure M2 = M2
  fun translate (M1.CardToSlot (c, slot)) = M2.CardToSlot (Tx.translate c, slot)
    | translate (M1.SlotToCard (slot, c)) = M2.SlotToCard (slot, Tx.translate c)
end
