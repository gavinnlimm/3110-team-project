(** Temporary game [Driver] for Cornell Krillion integration.

    {2 Ownership}
    - Primary scaffolding: Ryan (rls494) / terminal + integration
    - Must be replaced or adapted by: Karan (game sessions), Gavin (question
      bank), Vinson (matcher + history), Colin (scoring + stats)

    {2 Status}
    Hardcoded Cornell prompts, exact/alias matching, and fixed rarity points so
    [Terminal] / [App] are runnable before real modules land. See
    [docs/FUTURE_WORK.md].

    {2 Remaining work (by owner)}
    - Karan (km2253): replace session state / seed prompt selection with the
      real game-session engine (timed rounds via Lwt).
    - Gavin (gl568): load prompts from the shared question pack instead of
      [builtin_prompts]; wire default/custom bank selection.
    - Vinson (vc377): route [submit] through the real matcher (normalize,
      nicknames, edit distance); stop using exact/alias-only matching here.
    - Colin (czt4): supply [points] / [rarity_label] from rarity scoring;
      replace [placeholder_stats] with aggregates over saved history.
    - Ryan (rls494): keep [Game_driver] / [Terminal.Session] as the UI seam;
      swap [App] to pack the real driver once the above exist.

    Keep [Game_driver] (or [Terminal.Session]) as the integration boundary. *)

(** Shared driver shape used by [App] and tests. *)
module type Game_driver = sig
  type t

  val start : seed:int -> t
  val current_prompt : t -> Terminal.prompt_view option
  val submit : t -> string -> Terminal.feedback_view * t
  val results : t -> Terminal.game_results
end

(** Built-in temporary implementation of [Game_driver]. *)
include Game_driver