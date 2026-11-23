(** Existing fixed demo scoring and placeholder statistics. *)

let score (a : Pack.answer_entry) = (a.points, a.rarity_label)

let total_depth rounds =
  List.fold_left
    (fun acc (r : Terminal.round_result) -> acc + r.points)
    0 rounds

let placeholder_stats : Terminal.stats_view =
  {
    games_played = 0;
    best_depth = 0;
    average_depth = 0.0;
    note =
      Some
        "Stats will populate once game history saving is wired in \
         (Vinson/Colin).";
  }
