(** OUnit tests for [Scoring] and [Profile].

    {2 Ownership}
    - Primary: Colin (czt4)
    - Backup: Vinson (vc377)

    Every expected value below was worked out by hand first; the arithmetic is
    in the comment above each test. *)

open OUnit2
open Cornellion_Dune_Project
open Scoring

(** {1 Fixtures} *)

(* A made-up crowd of 200 students answering "Name a Cornell dining hall". The
   counts sit exactly on, or just under, each cutoff:
   - Okenshields 60/200 = 30.0%  -> exactly 30%, Common
   - RPCC        59/200 = 29.5%  -> just under 30%, Uncommon
   - Appel       31/200 = 15.5%  -> Uncommon
   - Morrison    20/200 = 10.0%  -> exactly 10%, Uncommon
   - Becker      19/200 =  9.5%  -> just under 10%, Rare
   - Bethe        6/200 =  3.0%  -> exactly 3%, Rare
   - Keeton       5/200 =  2.5%  -> just under 3%, Deep_cut
   - 104West!     0/200          -> nobody said it, Legend
   Total: 60 + 59 + 31 + 20 + 19 + 6 + 5 + 0 = 200. *)
let dining =
  [
    { canonical = "Okenshields"; count = 60; gem = false };
    { canonical = "RPCC"; count = 59; gem = false };
    { canonical = "Appel"; count = 31; gem = false };
    { canonical = "Morrison"; count = 20; gem = false };
    { canonical = "Becker"; count = 19; gem = false };
    { canonical = "Bethe"; count = 6; gem = false };
    { canonical = "Keeton"; count = 5; gem = false };
    { canonical = "104West!"; count = 0; gem = false };
  ]

let rated = rate_prompt dining

(** [tier_named name] is the tier [rate_prompt] gave [name] in [dining]. *)
let tier_named name =
  match score_answer rated name with
  | Some s -> s.tier
  | None -> failwith ("not in fixture: " ^ name)

let string_of_tier_opt t =
  match t with
  | Some t -> string_of_tier t
  | None -> "None"

(** [round points] is a round worth [points]; 0 is a miss. *)
let round points : Terminal.round_result =
  let tier = tier_of_points points in
  {
    prompt_text = "Prompt";
    typed = "typed";
    understood_as = (if points > 0 then Some "Answer" else None);
    points;
    rarity_label = Option.map string_of_tier tier;
  }

let miss = round 0
let common = round 10
let uncommon = round 30
let rare = round 60
let deep = round 85
let legend = round 100

(** [game rs] is a finished game with rounds [rs]. *)
let game rs : Terminal.game_results =
  { seed = 0; rounds = rs; total_depth = total_score rs }

let pp_achievements l =
  "[" ^ String.concat "; " (List.map Profile.name l) ^ "]"

let assert_achievements expected actual =
  assert_equal ~printer:pp_achievements expected actual

let has_substr haystack needle =
  let n = String.length needle in
  let h = String.length haystack in
  let rec loop i =
    if i + n > h then false
    else if String.sub haystack i n = needle then true
    else loop (i + 1)
  in
  n = 0 || loop 0

(** {1 Scoring: tiers and points} *)

let assert_tier name expected =
  name >:: fun _ ->
  assert_equal ~printer:string_of_tier expected (tier_named name)

(* each answer in [dining] lands in the tier worked out above *)
let boundary_tests =
  "tier boundaries on 200 students"
  >::: [
         assert_tier "Okenshields" Common;
         assert_tier "RPCC" Uncommon;
         assert_tier "Appel" Uncommon;
         assert_tier "Morrison" Uncommon;
         assert_tier "Becker" Rare;
         assert_tier "Bethe" Rare;
         assert_tier "Keeton" Deep_cut;
         assert_tier "104West!" Legend;
       ]

let scoring_tests =
  "scoring"
  >::: [
         (* 10, 30, 60, 85, 100, and each one maps back to its tier *)
         ( "points round trip" >:: fun _ ->
           assert_equal [ 10; 30; 60; 85; 100 ]
             (List.map points_of_tier all_tiers);
           List.iter
             (fun t ->
               assert_equal ~printer:string_of_tier_opt (Some t)
                 (tier_of_points (points_of_tier t)))
             all_tiers );
         (* 0 points is a miss, and 45 isn't any tier's value *)
         ( "no tier for 0 or 45" >:: fun _ ->
           assert_equal None (tier_of_points 0);
           assert_equal None (tier_of_points 45) );
         (* points follow the tier: Okenshields 10, Keeton 85, 104West! 100 *)
         ( "points from rate_prompt" >:: fun _ ->
           let pts name =
             match score_answer rated name with
             | Some s -> s.points
             | None -> -1
           in
           assert_equal 10 (pts "Okenshields");
           assert_equal 85 (pts "Keeton");
           assert_equal 100 (pts "104West!") );
         (* 60 / 200 * 100 = 30.0 percent, shown for display only *)
         ( "percent" >:: fun _ ->
           match score_answer rated "Okenshields" with
           | Some s -> assert_equal ~printer:string_of_float 30.0 s.percent
           | None -> assert_failure "missing Okenshields" );
         (* an answer that isn't in the prompt is a miss *)
         ( "unknown answer" >:: fun _ ->
           assert_equal None (score_answer rated "Terrace") );
         (* a gem is Legend even at 60/200 = 30%, which would be Common *)
         ( "gem overrides count" >:: fun _ ->
           let r =
             rate_prompt
               [
                 { canonical = "Okenshields"; count = 60; gem = true };
                 { canonical = "RPCC"; count = 140; gem = false };
               ]
           in
           assert_equal
             [ (Legend, 100); (Common, 10) ]
             (List.map (fun s -> (s.tier, s.points)) r) );
         (* an empty prompt scores nothing and doesn't divide by zero *)
         ("empty prompt" >:: fun _ -> assert_equal [] (rate_prompt []));
         (* every count is 0, so the total is 0: all Legend, 0 percent *)
         ( "all zero counts" >:: fun _ ->
           let r =
             rate_prompt
               [
                 { canonical = "A"; count = 0; gem = false };
                 { canonical = "B"; count = 0; gem = false };
               ]
           in
           assert_equal [ Legend; Legend ] (List.map (fun s -> s.tier) r);
           assert_equal [ 0.; 0. ] (List.map (fun s -> s.percent) r) );
         (* a negative count is treated as 0, so it is Legend and the total
            stays 10: B is 10/10 = 100%, Common *)
         ( "negative count" >:: fun _ ->
           let r =
             rate_prompt
               [
                 { canonical = "A"; count = -5; gem = false };
                 { canonical = "B"; count = 10; gem = false };
               ]
           in
           assert_equal [ (Legend, 0); (Common, 10) ]
             (List.map (fun s -> (s.tier, s.times_given)) r) );
         (* the rarest three: 104West! (100 pts), Keeton (85), then the two
            Rare answers tie at 60 and Bethe (6 people) beats Becker (19) *)
         ( "rarest" >:: fun _ ->
           assert_equal
             ~printer:(String.concat ", ")
             [ "104West!"; "Keeton"; "Bethe" ]
             (List.map (fun s -> s.answer) (rarest rated 3));
           assert_equal 8 (List.length (rarest rated 20)) );
       ]

(** {1 Scoring: totals, depth, and band} *)

let game_tests =
  "totals, depth, band"
  >::: [
         (* 10 + 60 + 100 = 170 points, and 170 * 10 = 1700 m *)
         ( "total and depth" >:: fun _ ->
           let rs = [ common; rare; legend ] in
           assert_equal 170 (total_score rs);
           assert_equal 1700 (depth rs) );
         (* a perfect 3-round game is 3 * 100 * 10 = 3000 m *)
         ("max depth" >:: fun _ -> assert_equal 3000 (max_depth 3));
         (* 3 rounds: under 90 total is Common, under 180 Uncommon, under 255
            Rare, under 300 Deep_cut, 300 Legend *)
         ( "band cutoffs" >:: fun _ ->
           let b rs = string_of_tier_opt (band rs) in
           (* 10 + 10 + 60 = 80 < 90 *)
           assert_equal ~printer:Fun.id "Tourist" (b [ common; common; rare ]);
           (* 30 + 30 + 30 = 90, exactly the Uncommon cutoff *)
           assert_equal ~printer:Fun.id "Freshman"
             (b [ uncommon; uncommon; uncommon ]);
           (* 10 + 60 + 100 = 170 < 180 *)
           assert_equal ~printer:Fun.id "Freshman" (b [ common; rare; legend ]);
           (* 85 * 3 = 255, exactly the Deep_cut cutoff *)
           assert_equal ~printer:Fun.id "Grad Student" (b [ deep; deep; deep ]);
           (* 100 + 100 + 85 = 285 < 300 *)
           assert_equal ~printer:Fun.id "Grad Student"
             (b [ legend; legend; deep ]);
           assert_equal ~printer:Fun.id "Ezra" (b [ legend; legend; legend ]);
           assert_equal ~printer:Fun.id "None" (b []) );
       ]

(** {1 Profile} *)

let profile_tests =
  "profile"
  >::: [
         (* a game with no rounds (input ended early) isn't counted *)
         ( "empty game ignored" >:: fun _ ->
           let p, fresh = Profile.update Profile.empty (game []) in
           assert_equal 0 p.games_played;
           assert_achievements [] fresh );
         (* Legend, Deep, Deep = 100 + 85 + 85 = 270 points, 2700 m. It is
            finished, all accepted, has a Legend, three Deep_cut-or-better in a
            row, no Tourists, and 270 >= 85 * 3 = 255. Not ten games yet. *)
         ( "great first game" >:: fun _ ->
           let p, fresh =
             Profile.update Profile.empty (game [ legend; deep; deep ])
           in
           assert_equal 1 p.games_played;
           assert_equal 270 p.high_score;
           assert_equal 2700 p.best_depth;
           assert_achievements
             [
               Profile.First_descent;
               Profile.Clean_run;
               Profile.Gem_hunter;
               Profile.Deep_streak;
               Profile.Off_the_path;
               Profile.Bottom_of_slope;
             ]
             fresh;
           assert_achievements fresh p.achievements );
         (* Common, miss, Rare = 70 points: only First_descent, since the miss
            breaks Clean_run and Off_the_path *)
         ( "game with a miss" >:: fun _ ->
           let _, fresh =
             Profile.update Profile.empty (game [ common; miss; rare ])
           in
           assert_achievements [ Profile.First_descent ] fresh );
         (* all accepted but one Tourist: Clean_run yes, Off_the_path no *)
         ( "tourist blocks off the path" >:: fun _ ->
           let _, fresh =
             Profile.update Profile.empty (game [ common; rare; rare ])
           in
           assert_achievements [ Profile.First_descent; Profile.Clean_run ] fresh );
         (* the same great game twice unlocks nothing new the second time, and
            the list has no duplicates *)
         ( "no duplicates" >:: fun _ ->
           let g = game [ legend; deep; deep ] in
           let p1, _ = Profile.update Profile.empty g in
           let p2, fresh = Profile.update p1 g in
           assert_achievements [] fresh;
           assert_equal 6 (List.length p2.achievements);
           assert_equal 2 p2.games_played );
         (* Deep, Deep, Rare, Deep: longest run is 2, so no Deep_streak *)
         ( "broken streak" >:: fun _ ->
           let p = Profile.empty in
           let g = game [ deep; deep; rare; deep ] in
           assert_bool "no streak"
             (not (Profile.earned Profile.Deep_streak { p with games_played = 1 } g)) );
         (* Rare, Deep, Deep, Legend: the last three are a run of 3 *)
         ( "streak at the end" >:: fun _ ->
           let p = { Profile.empty with games_played = 1 } in
           assert_bool "streak"
             (Profile.earned Profile.Deep_streak p (game [ rare; deep; deep; legend ]))
         );
         (* nine games isn't Veteran, and the tenth game unlocks it *)
         ( "veteran on tenth game" >:: fun _ ->
           let nine = Profile.of_history (List.init 9 (fun _ -> game [ rare ])) in
           assert_equal 9 nine.games_played;
           assert_bool "not yet" (not (List.mem Profile.Veteran nine.achievements));
           let ten, fresh = Profile.update nine (game [ rare ]) in
           assert_equal 10 ten.games_played;
           assert_achievements [ Profile.Veteran ] fresh );
         (* games of 100 and 50 points: high score 100, average (100 + 50) / 2
            = 75.0, average depth 750.0 m, best depth 1000 m *)
         ( "averages" >:: fun _ ->
           let p = Profile.of_history [ game [ legend ]; game [ uncommon; common; common ] ] in
           assert_equal 100 p.high_score;
           assert_equal 1000 p.best_depth;
           assert_equal ~printer:string_of_float 75.0 (Profile.average_score p);
           assert_equal ~printer:string_of_float 750.0 (Profile.average_depth p)
         );
         (* no games: averages are 0, not a division by zero *)
         ( "empty averages" >:: fun _ ->
           assert_equal 0. (Profile.average_score Profile.empty);
           assert_equal 0. (Profile.average_depth Profile.empty) );
         (* the stats screen gets the profile's numbers and the note *)
         ( "stats view" >:: fun _ ->
           let p = Profile.of_history [ game [ legend ]; game [ uncommon; common; common ] ] in
           let v = Profile.to_stats_view p in
           assert_equal 2 v.games_played;
           assert_equal 1000 v.best_depth;
           assert_equal 750.0 v.average_depth;
           match v.note with
           | Some n -> assert_bool "high score" (has_substr n "High score: 100")
           | None -> assert_failure "no note" );
       ]

let () =
  run_test_tt_main
    ("scoring and profile"
    >::: [ boundary_tests; scoring_tests; game_tests; profile_tests ])
