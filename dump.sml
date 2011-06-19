structure Dump = struct
  fun intstring n = if n < 0 then "-" ^ Int.toString (~n) else Int.toString n

  fun slots a =
      (* flagrantly violates single point of truth *)
    let fun dump i = case Array.sub (a, i)
                       of (10000, Term.C Term.I) => () (* do nothing *)
                        | (v, f) => app print [intstring i, "={",
                                               intstring v, ",",
                                               ShowTerm.show f, "}\n"]
        fun all 256 = ()
          | all i = (dump i; all (i + 1))
    in  all 0
    end
end
