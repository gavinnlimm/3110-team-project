(** Game sessions for Cornell Krillion: one game from the first prompt to the
    results screen.

    {2 Ownership}
    - Primary: Karan (km2253) — game sessions
    - Backup: Ryan (rls494)

    {2 Status}
    MS2: implements [Driver.Game_driver], so [App] runs real games through it.
    A game draws its questions with [Pack.select], matches each typed answer
    against the question's names and nicknames using [Matcher]'s
    normalization, scores accepted answers with [Scoring.rate_prompt], and
    records every round for the results screen and [Profile]. Tested in
    [test/test_session.ml].

    {2 Remaining work}
    - Karan: timed rounds with Lwt (a timeout scores 0 and moves on); switch
      matching to Vinson's fuzzy matcher once it lands; hand finished games to
      history saving and [Profile.update].

    {2 Rules}
    - A game has [min rounds (Pack.size pack)] rounds, asked in the order
      [Pack.select ~seed] gives, with no question repeated.
    - Every submission ends the current round. A valid answer earns its rarity
      tier's points; an invalid or empty answer earns 0.
    - [results] reports [total_depth] in meters, [Scoring.depth] of the rounds
      played (total points * [Scoring.meters_per_point]), the same unit that
      [Profile] uses for best depth.

    See [docs/FUTURE_WORK.md]. *)

include Driver.Game_driver
(** [start ~seed] begins a game of [rounds_per_game] rounds from the default
    pack. [current_prompt s] is the prompt to answer now, or [None] once the
    game is over. [submit s typed] scores [typed] for the current prompt and
    moves to the next one; after the game is over it changes nothing and
    returns rejected feedback. [results s] summarizes the rounds played so
    far, oldest first. *)

val rounds_per_game : int
(** [rounds_per_game] is how many questions [start] asks: 5. *)

val start_with : seed:int -> rounds:int -> Pack.t -> t
(** [start_with ~seed ~rounds pack] begins a game of [min rounds (Pack.size
    pack)] questions from [pack]. Used by tests and, later, by custom packs.
    Raises [Invalid_argument] if [rounds < 0]. *)

val is_over : t -> bool
(** [is_over s] is whether every question in [s] has been answered. *)

val match_answer : Pack.question -> string -> Pack.answer option
(** [match_answer q typed] is the answer of [q] whose name or a nickname equals
    [typed] after [Matcher.normalize], checking answers in pack order, or
    [None] if there is none. *)

val score : Pack.question -> Pack.answer -> Scoring.scored_answer
(** [score q a] is [a]'s rarity tier and points, rated against the counts of
    all of [q]'s answers. Requires: [a] is one of [q]'s answers. *)
