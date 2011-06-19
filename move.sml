signature MOVE = sig
  structure Card : CARD
  type card = Card.unitype Card.card
  datatype t = CardToSlot of card * int
             | SlotToCard of int * card
end

functor Move (Card : CARD) : MOVE = struct
  structure Card = Card
  type card = Card.unitype Card.card
  datatype t = CardToSlot of card * int
             | SlotToCard of int * card
end
