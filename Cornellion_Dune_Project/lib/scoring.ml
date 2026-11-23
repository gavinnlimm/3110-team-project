type tier =
  | Common
  | Uncommon
  | Rare
  | Deep_cut
  | Legend

type answer_count = {
  canonical : string;
  count : int;
  gem : bool;
}

type scored_answer = {
  answer : string;
  times_given : int;
  percent : float;
  tier : tier;
  points : int;
}

let all_tiers = [ Common; Uncommon; Rare; Deep_cut; Legend ]

let points_of_tier t =
  match t with
  | Common -> 10
  | Uncommon -> 30
  | Rare -> 60
  | Deep_cut -> 85
  | Legend -> 100

let tier_of_points p =
  List.find_opt (fun t -> points_of_tier t = p) all_tiers

let string_of_tier t =
  match t with
  | Common -> "Tourist"
  | Uncommon -> "Freshman"
  | Rare -> "Upperclassman"
  | Deep_cut -> "Grad Student"
  | Legend -> "Ezra"

(* Compares count / total against a percent cutoff without floats:
   count / total >= pct / 100 exactly when count * 100 >= total * pct. *)
let tier_of_count count total =
  if count <= 0 then Legend
  else if count * 100 >= total * 30 then Common
  else if count * 100 >= total * 10 then Uncommon
  else if count * 100 >= total * 3 then Rare
  else Deep_cut

let rate_prompt answers =
  (* [clamp n] is [n], or 0 if [n] is negative. *)
  let clamp n = if n < 0 then 0 else n in
  let total =
    List.fold_left (fun acc a -> acc + clamp a.count) 0 answers
  in
  (* [rate a] is [a] scored against [total]. *)
  let rate a =
    let count = clamp a.count in
    let tier = if a.gem then Legend else tier_of_count count total in
    let percent =
      if total = 0 then 0.
      else float_of_int count /. float_of_int total *. 100.
    in
    {
      answer = a.canonical;
      times_given = count;
      percent;
      tier;
      points = points_of_tier tier;
    }
  in
  List.map rate answers

let score_answer rated canonical =
  List.find_opt (fun s -> s.answer = canonical) rated

let rarest rated n =
  (* higher points first; among equal points, fewer people first *)
  let compare_rarity a b =
    if a.points <> b.points then compare b.points a.points
    else compare a.times_given b.times_given
  in
  let sorted = List.stable_sort compare_rarity rated in
  List.filteri (fun i _ -> i < n) sorted

let meters_per_point = 10

let total_score rounds =
  List.fold_left
    (fun acc (r : Terminal.round_result) -> acc + r.points)
    0 rounds

let depth rounds = total_score rounds * meters_per_point
let max_depth n = n * points_of_tier Legend * meters_per_point

let band rounds =
  let n = List.length rounds in
  if n = 0 then None
  else
    (* average = total / n, compared as total against cutoff * n *)
    let total = total_score rounds in
    if total < 30 * n then Some Common
    else if total < 60 * n then Some Uncommon
    else if total < 85 * n then Some Rare
    else if total < 100 * n then Some Deep_cut
    else Some Legend
