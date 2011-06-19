structure UT = struct
  structure Move = MoveFn(StringCard)
  structure U = UsefulFn(Move)
  structure IO = MoveIOFn(structure Move = Move val prompt = false)
  fun show moves = app (IO.printMove "") moves

  fun attack slot i j n = show (U.attack slot i j n)
  fun help   slot i j n = show (U.help   slot i j n)

  fun mutual s1 s2 n = show (U.mutualHelpUsing s1 s2 s2 n)

end
