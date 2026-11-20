(** Temporary MVP game driver for terminal integration.

    This module is scaffolding owned by the terminal/integration workstream. It
    exists so the UI is runnable and testable before the real question bank,
    matcher, scoring, and game-session modules land.

    Replace [Mvp_driver] with adapters around those modules once they exist;
    keep [Game_driver] (or [Terminal.Session]) as the seam. *)

(** Shared driver shape used by [App] and tests. *)
module type Game_driver = sig
  type t

  val start : seed:int -> t
  val current_prompt : t -> Terminal.prompt_view option
  val submit : t -> string -> Terminal.feedback_view * t
  val results : t -> Terminal.game_results
end

(** Hardcoded Cornell prompts, exact/alias matching, fixed rarity points. *)
module Mvp_driver : Game_driver

(** Placeholder personal stats until saved-history lands. *)
val placeholder_stats : Terminal.stats_view
