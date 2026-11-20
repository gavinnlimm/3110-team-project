(** Application loop: menus + play session wiring for Cornell Krillion.

    [App] connects [Terminal] screens to a [Mvp_driver.Game_driver]. When real
    game-session / pack / matcher / scoring modules land, swap the driver used
    inside [run] (or parameterize it) without changing the screen layer. *)

val run : Terminal.io -> unit
(** Run the interactive menu until Quit or EOF. *)

val play_with_driver :
  Terminal.io ->
  seed:int ->
  (module Mvp_driver.Game_driver with type t = 'a) ->
  Terminal.game_results
(** Start a driver session, run the play loop, print results, return them. *)
