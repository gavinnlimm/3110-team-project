(** Terminal screens and scriptable I/O for Cornell Krillion.

    {2 Ownership}
    - Primary: Ryan (rls494) — terminal interface + integration
    - Backup: Colin (czt4)

    {2 Status}
    Presentation only: view models, pure string renderers, and helpers that
    read/write through an injectable [io] record. Game logic (matching,
    scoring, sessions, packs) must stay out of this module and enter via
    [Session] / [Driver.Game_driver].

    {2 Remaining work (by owner)}
    - Ryan: keep renderers stable; add timed-round UX hooks when Karan’s Lwt
      session can interrupt [read_answer]; optional richer TUI later.
    - Gavin: map pack prompts into [prompt_view]; drive bank menu from real
      default/custom loaders (UI already has [render_bank_menu]).
    - Vinson: map matcher outcomes into [feedback_view] /
      [round_result.understood_as].
    - Colin: map rarity tiers/points into [feedback_view] / [round_result];
      fill [stats_view] from real aggregates (not placeholder notes).
    - Karan: implement [Session] (or adapt to it) so [run_play_session] drives
      the real engine; timeouts should produce 0-point feedback.

    See [docs/FUTURE_WORK.md]. *)

(** {1 View models}

    Teammate modules should map their domain types into these records before
    calling a renderer. Keeping the UI on view models avoids coupling the
    terminal to unfinished pack / matcher / scoring APIs. *)

(** One prompt shown to the player during a round. *)
type prompt_view = {
  index : int;  (** 1-based round number *)
  total : int;  (** total rounds in this game *)
  text : string;  (** prompt text, e.g. "Name a Cornell dining hall" *)
}

(** Immediate feedback after the player submits an answer. *)
type feedback_view = {
  accepted : bool;
  typed : string;  (** raw input from the player *)
  understood_as : string option;
      (** canonical answer when accepted; [None] when rejected *)
  points : int;
  rarity_label : string option;
      (** e.g. "Common", "Uncommon", "Rare"; [None] when rejected *)
}

(** One completed round, used on the results screen. *)
type round_result = {
  prompt_text : string;
  typed : string;
  understood_as : string option;
  points : int;
  rarity_label : string option;
}

(** End-of-game summary. *)
type game_results = {
  seed : int;
  rounds : round_result list;
  total_depth : int;  (** sum of points across rounds *)
}

(** Personal statistics screen data. Until the history module lands, callers may
    pass empty/placeholder values. *)
type stats_view = {
  games_played : int;
  best_depth : int;
  average_depth : float;
  note : string option;  (** optional scaffold message *)
}

(** Top-level menu selections. *)
type menu_choice =
  | Play
  | Stats
  | Bank
  | Quit

(** Question-bank menu selections (scaffold for future pack loading). *)
type bank_choice =
  | Default_bank
  | Custom_bank
  | Back

(** {1 Pure screen renderers}

    These functions allocate strings only; they perform no I/O. Prefer them in
    unit tests. *)

val render_main_menu : unit -> string
(** Title + numbered options for Play, Stats, Question bank, Quit. *)

val render_bank_menu : unit -> string
(** Default vs custom question-bank selection (custom is a future stub). *)

val render_prompt : prompt_view -> string
(** Round header and prompt text asking for an answer. *)

val render_feedback : feedback_view -> string
(** Accepted/rejected message with points when applicable. *)

val render_results : game_results -> string
(** Per-round breakdown and total depth. *)

val render_stats : stats_view -> string
(** Personal statistics summary. *)

(** {1 Scriptable I/O} *)

(** Injectable print/read so tests can drive the UI without a real terminal. *)
type io = {
  print_line : string -> unit;
  read_line : unit -> string option;
      (** [None] means EOF / no more scripted input *)
}

val stdio : io
(** [print_line]/stdout; [read_line] from stdin ([None] at EOF). *)

val print_screen : io -> string -> unit
(** Write a multi-line screen, ensuring a trailing blank line for readability. *)

(** {1 Interaction helpers} *)

val parse_menu_choice : string -> menu_choice option
(** Parse a main-menu selection; accepts numbers or case-insensitive keywords. *)

val parse_bank_choice : string -> bank_choice option

val read_menu_choice : io -> menu_choice
(** Prompt until a valid [menu_choice] is entered (or EOF → [Quit]). *)

val read_bank_choice : io -> bank_choice
(** Prompt until a valid [bank_choice] is entered (or EOF → [Back]). *)

val read_answer : io -> string option
(** Read one answer line; [None] on EOF. Empty strings are returned as [Some ""].
*)

val read_seed : io -> int
(** Ask for a game seed; default [0] on blank/EOF/invalid input. *)

(** Minimal session driver used by [run_play_session]. Real game sessions should
    implement this shape (or adapt into it) when their module lands. *)
module type Session = sig
  type t

  val current_prompt : t -> prompt_view option
  val submit : t -> string -> feedback_view * t
  val results : t -> game_results
end

val run_play_session : io -> (module Session with type t = 'a) -> 'a -> game_results
(** Show prompts, collect answers, print feedback, return final results. Stops
    early if input hits EOF (remaining rounds scored as skipped/empty by the
    session implementation, or simply unfinished—callers should use a complete
    script in tests). *)