structure Term = struct
  datatype 'a t
    = I | S | K | put
    | zero | succ | dbl
    | get | copy
    | inc | dec | attack | help | revive | zombie

    | P1 of 'a partial1  (* partial applications of a term *)
    | P2 of 'a partial2
  withtype 'a partial1 = 'a t * 'a t
      and  'a partial2 = 'a partial1 * 'a t
end
