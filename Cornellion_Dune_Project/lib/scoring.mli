(** Rarity scoring for Cornell Krillion: turns each prompt's simulated crowd
    counts into rarity tiers, points, depth down the slope, and a game band.

    {2 Ownership}
    - Primary: Colin (czt4) — crowd data and rarity scoring
    - Backup: Vinson (vc377)

    {2 Status}
    Pure functions only. Takes crowd counts in its own small [answer_count]
    type, so the question pack (Gavin) only has to supply a canonical name and
    a count for each answer. All tier cutoffs use integer arithmetic, so an
    answer exactly on a boundary always lands in the same tier.

    See [docs/FUTURE_WORK.md]. *)

(** How rare an accepted answer is, from most common to rarest. *)
type tier =
  | Common  (** at least 30% of the crowd gave it *)
  | Uncommon  (** at least 10% but under 30% *)
  | Rare  (** at least 3% but under 10% *)
  | Deep_cut  (** said by someone, but under 3% *)
  | Legend
      (** nobody in the crowd said it, or the pack hand-picked it as the gem *)

type answer_count = {
  canonical : string;
  count : int;
  gem : bool;
}
(** One valid answer to a prompt: its [canonical] name, how many people in the
    simulated crowd gave it, and whether the pack marks it as the prompt's gem.
*)

type scored_answer = {
  answer : string;
  times_given : int;
  percent : float;
  tier : tier;
  points : int;
}
(** One valid answer after scoring: its name, its crowd count, the percent of
    the crowd that gave it (for display only), its [tier], and its [points]. *)

val all_tiers : tier list
(** [all_tiers] is every tier from [Common] to [Legend]. *)

val points_of_tier : tier -> int
(** [points_of_tier t] is 10, 30, 60, 85, or 100 for [Common] through
    [Legend]. *)

val tier_of_points : int -> tier option
(** [tier_of_points p] is the tier worth exactly [p] points, or [None] if no
    tier is worth [p] (for example 0 for a missed answer). *)

val string_of_tier : tier -> string
(** [string_of_tier t] is the label shown to players: "Tourist", "Freshman",
    "Upperclassman", "Grad Student", or "Ezra" for [Common] through [Legend]. *)

val tier_of_count : int -> int -> tier
(** [tier_of_count count total] is the tier of an answer given by [count] out of
    [total] people. A [count] of 0 is [Legend]. Requires: [0 <= count <= total].
*)

val rate_prompt : answer_count list -> scored_answer list
(** [rate_prompt answers] scores every answer of one prompt against the total
    count of [answers]. A gem is always [Legend]. The result keeps the order of
    [answers]. Negative counts are treated as 0. *)

val score_answer : scored_answer list -> string -> scored_answer option
(** [score_answer rated canonical] is the scored answer named [canonical] in
    [rated], or [None] if it is not there (a miss, worth 0 points). *)

val rarest : scored_answer list -> int -> scored_answer list
(** [rarest rated n] is the [n] highest-scoring answers in [rated], rarest
    first, for a "you could have said..." hint. Fewer if [rated] is shorter. *)

val meters_per_point : int
(** [meters_per_point] is how far the bear slides for each point: 10. *)

val total_score : Terminal.round_result list -> int
(** [total_score rounds] is the sum of the points in [rounds]. *)

val depth : Terminal.round_result list -> int
(** [depth rounds] is how many meters down the slope [rounds] take the bear:
    [total_score rounds * meters_per_point]. *)

val max_depth : int -> int
(** [max_depth n] is the depth of a perfect game of [n] rounds, every answer a
    [Legend]. *)

val band : Terminal.round_result list -> tier option
(** [band rounds] rates a whole game by its average points per round: under 30
    is [Common], under 60 [Uncommon], under 85 [Rare], under 100 [Deep_cut],
    and 100 [Legend]. [None] if [rounds] is empty. *)
val score : Pack.answer_entry -> int * string
(** Fixed scoring for the existing demo question bank. *)

val total_depth : Terminal.round_result list -> int
val placeholder_stats : Terminal.stats_view
