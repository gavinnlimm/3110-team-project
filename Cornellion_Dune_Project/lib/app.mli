(** Application loop: menus + play-session wiring for Cornell Krillion.

    {2 Ownership}
    - Primary: Ryan (rls494) — terminal + integration
    - Backup: Colin (czt4)

    {2 Status}
    Wires [Terminal] screens to a [Driver.Game_driver]. Currently packs the
    temporary [Driver] implementation.

    {2 Remaining work (by owner)}
    - Ryan: swap [play_with_driver] / [handle_play] to use Karan’s session (or
      an adapter) once it implements [Game_driver]; remove dependence on stub
      pack/match/score inside [Driver].
    - Gavin: [handle_bank] Default should load the real pack; Custom should call
      the pack importer (today: placeholder messages only).
    - Vinson + Colin: [handle_stats] should render real aggregates from history
      instead of [Driver.placeholder_stats].
    - Karan: timed sessions may require [App] to use an async-aware play loop
      once Lwt timers exist.

    See [docs/FUTURE_WORK.md]. *)

val run : Terminal.io -> unit
(** Run the interactive menu until Quit or EOF. *)

val play_with_driver :
  Terminal.io ->
  seed:int ->
  (module Driver.Game_driver with type t = 'a) ->
  Terminal.game_results
(** Start a driver session, run the play loop, print results, return them. *)