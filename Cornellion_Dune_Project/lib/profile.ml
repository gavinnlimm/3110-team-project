type achievement =
  | First_descent
  | Clean_run
  | Gem_hunter
  | Deep_streak
  | Off_the_path
  | Bottom_of_slope
  | Veteran

type t = {
  games_played : int;
  high_score : int;
  best_depth : int;
  total_score : int;
  achievements : achievement list;
}

let empty =
  {
    games_played = 0;
    high_score = 0;
    best_depth = 0;
    total_score = 0;
    achievements = [];
  }

let all_achievements =
  [
    First_descent;
    Clean_run;
    Gem_hunter;
    Deep_streak;
    Off_the_path;
    Bottom_of_slope;
    Veteran;
  ]

let name a =
  match a with
  | First_descent -> "First Descent"
  | Clean_run -> "Clean Run"
  | Gem_hunter -> "Gem Hunter"
  | Deep_streak -> "Deep Streak"
  | Off_the_path -> "Off the Beaten Path"
  | Bottom_of_slope -> "Bottom of the Slope"
  | Veteran -> "Veteran"

let description a =
  match a with
  | First_descent -> "Finish your first game."
  | Clean_run -> "Get every answer in a game accepted."
  | Gem_hunter -> "Give an Ezra answer."
  | Deep_streak -> "Give three Grad Student or better answers in a row."
  | Off_the_path -> "Finish a game with every answer accepted and no Tourists."
  | Bottom_of_slope -> "Average at least 85 points per round in a game."
  | Veteran -> "Play ten games."

let earned a p (g : Terminal.game_results) =
  let rounds = g.rounds in
  let n = List.length rounds in
  (* [tier_of r] is the tier of round [r], or [None] if it was a miss. *)
  let tier_of (r : Terminal.round_result) = Scoring.tier_of_points r.points in
  let accepted r = tier_of r <> None in
  (* [longest_deep rs run best] is the longest run of Deep_cut-or-better rounds
     in [rs], given the current [run] length and the [best] run so far. *)
  let rec longest_deep rs run best =
    match rs with
    | [] -> max run best
    | r :: rest -> (
        match tier_of r with
        | Some Scoring.Deep_cut | Some Scoring.Legend ->
            longest_deep rest (run + 1) best
        | _ -> longest_deep rest 0 (max run best))
  in
  match a with
  | First_descent -> n > 0
  | Clean_run -> n > 0 && List.for_all accepted rounds
  | Gem_hunter -> List.exists (fun r -> tier_of r = Some Scoring.Legend) rounds
  | Deep_streak -> longest_deep rounds 0 0 >= 3
  | Off_the_path ->
      n > 0
      && List.for_all
           (fun r ->
             match tier_of r with
             | None | Some Scoring.Common -> false
             | Some _ -> true)
           rounds
  | Bottom_of_slope -> n > 0 && Scoring.total_score rounds >= 85 * n
  | Veteran -> p.games_played >= 10

let update p (g : Terminal.game_results) =
  if g.rounds = [] then (p, [])
  else
    let score = Scoring.total_score g.rounds in
    let depth = Scoring.depth g.rounds in
    let counted =
      {
        p with
        games_played = p.games_played + 1;
        high_score = max p.high_score score;
        best_depth = max p.best_depth depth;
        total_score = p.total_score + score;
      }
    in
    let fresh =
      List.filter
        (fun a -> (not (List.mem a p.achievements)) && earned a counted g)
        all_achievements
    in
    ({ counted with achievements = p.achievements @ fresh }, fresh)

let of_history games =
  List.fold_left (fun p g -> fst (update p g)) empty games

let average_score p =
  if p.games_played = 0 then 0.
  else float_of_int p.total_score /. float_of_int p.games_played

let average_depth p = average_score p *. float_of_int Scoring.meters_per_point

let to_stats_view p : Terminal.stats_view =
  {
    games_played = p.games_played;
    best_depth = p.best_depth;
    average_depth = average_depth p;
    note =
      Some
        (Printf.sprintf "High score: %d   Achievements: %d of %d" p.high_score
           (List.length p.achievements)
           (List.length all_achievements));
  }
