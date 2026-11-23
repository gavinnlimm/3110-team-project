(** The player's profile for Cornell Krillion: games played, high score, best
    depth, average, and unlocked achievements.

    {2 Ownership}
    - Primary: Colin (czt4) — scoring, achievements, and stats
    - Backup: Vinson (vc377)

    {2 Status}
    Pure: a profile is never changed in place. [update] returns a new profile
    after each finished game, and [of_history] rebuilds one from saved games,
    so saving and loading only has to store [Terminal.game_results] (Vinson's
    history file). Tiers are read back from each round's points with
    [Scoring.tier_of_points].

    See [docs/FUTURE_WORK.md]. *)

(** Something the player can unlock once. *)
type achievement =
  | First_descent  (** finish a game *)
  | Clean_run  (** every answer in a game accepted *)
  | Gem_hunter  (** an [Ezra] ([Legend]) answer *)
  | Deep_streak  (** three [Grad Student] or better answers in a row *)
  | Off_the_path  (** every answer accepted and none a [Tourist] *)
  | Bottom_of_slope  (** a game averaging at least 85 points per round *)
  | Veteran  (** ten games played *)

type t = {
  games_played : int;
  high_score : int;
  best_depth : int;
  total_score : int;
  achievements : achievement list;
}
(** A player's record: how many games they finished, their highest score and
    deepest depth in meters, the sum of all their scores (for the average), and
    their achievements in the order they were unlocked. *)

val empty : t
(** [empty] is the profile of a player who has not played yet. *)

val all_achievements : achievement list
(** [all_achievements] is every achievement, in the order shown on screen. *)

val name : achievement -> string
(** [name a] is the short title of [a], e.g. "Gem Hunter". *)

val description : achievement -> string
(** [description a] is one line saying how to unlock [a]. *)

val earned : achievement -> t -> Terminal.game_results -> bool
(** [earned a p g] is whether game [g] (or the totals in [p]) meets [a]'s rule.
    Requires: [p] already counts [g]. *)

val update : t -> Terminal.game_results -> t * achievement list
(** [update p g] is [p] with game [g] added, paired with the achievements [g]
    unlocked that [p] did not already have. A game with no rounds (input ended
    before the first answer) is not counted: the result is [(p, [])]. *)

val of_history : Terminal.game_results list -> t
(** [of_history games] is the profile after playing [games] in order, oldest
    first, starting from [empty]. *)

val average_score : t -> float
(** [average_score p] is [p]'s mean score per game, or [0.] with no games. *)

val average_depth : t -> float
(** [average_depth p] is [p]'s mean depth in meters per game, or [0.] with no
    games. *)

val to_stats_view : t -> Terminal.stats_view
(** [to_stats_view p] fills the terminal's statistics screen from [p], with the
    high score and achievement count in its note. *)
