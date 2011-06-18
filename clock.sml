signature CLOCK = sig
  type t  (* mutable clock *)
  val mk : unit -> t  (* fresh clock *)
  val reset : t -> unit (* make clock fresh *)
  exception TimeExpired
  val tick : t -> ('a -> 'b) -> ('a -> 'b)
    (* makes function application tick, may raise TimeExpired *)
end

structure Clock :> CLOCK = struct
  type t = { apps_left : int ref } (* number of applications remaining *)
  val stdClock = 1000  (* number of ticks on a standard clock *)
  fun mk () = { apps_left = ref stdClock }
  fun reset { apps_left } = apps_left := stdClock

  exception TimeExpired
  fun tick { apps_left } f a =
    if !apps_left > 0 then
      f a before apps_left := !apps_left - 1
    else
      raise TimeExpired

end
