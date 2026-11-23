(** OUnit tests for [Session]: full games with fixed seeds and fixed answers.

    {2 Ownership}
    - Primary: Karan (km2253)
    - Backup: Ryan (rls494)

    {2 How the expected values were found}
    Every expected point value is worked out by hand from the counts in the
    pack and the tier cutoffs in [scoring.mli] (Common at least 30%, Uncommon
    at least 10%, Rare at least 3%, otherwise Grad Student). Tests never take
    an expected number from running [Session] itself. Where the order of
    questions matters, the expected order comes from [Pack.select], which
    [test/test_pack.ml] checks separately. *)

open OUnit2
open Cornellion_Dune_Project

(** {1 Fixtures} *)

(* Hand-worked tiers:
   fruit (total 100): Apple 50% Common 10; Banana exactly 30% Common 10;
     Cherry 15% Uncommon 30; Durian 4% Rare 60; Elderberry 1% Grad Student 85.
   color (total 10): Red 90% Common 10; Blue exactly 10% Uncommon 30.
   planet (total 4): Earth 75% Common 10; Mars 25% Uncommon 30. *)
let test_pack_text =
  {|pack: Session test pack

question: fruit
prompt: Name a fruit
answer: Apple | 50 | Apples
answer: Banana | 30
answer: Cherry | 15
answer: Durian | 4 | Stinky Fruit
answer: Elderberry | 1

question: color
prompt: Name a color
answer: Red | 9
answer: Blue | 1 | Navy Blue

question: planet
prompt: Name a planet
answer: Earth | 3
answer: Mars | 1
|}

let color_only_text =
  {|question: color
prompt: Name a color
answer: Red | 9
answer: Blue | 1 | Navy Blue
|}

let pack_of text =
  match Pack.of_string text with
  | Ok p -> p
  | Error e -> failwith ("bad test pack: " ^ Pack.error_to_string e)

let test_pack = pack_of test_pack_text
let color_pack = pack_of color_only_text

let question pack id =
  match Pack.find pack id with
  | Some q -> q
  | None -> failwith ("no question " ^ id)

let answer (q : Pack.question) name =
  List.find (fun (a : Pack.answer) -> a.canonical = name) q.answers

(* For each default-pack prompt: the rarest answer as a player might type it,
   and its points worked out by hand (counts are out of 300; library is out
   of 314).
   dining-hall: 104West! 10/300 = 3.3%, at least 3% -> Rare 60
   engineering-quad: Bard 6/300 = 2% -> Grad Student 85
   tradition: Convocation 12/300 = 4% -> Rare 60
   library: Africana 6/314 = 1.9% -> Grad Student 85
   college: Bowers 4/300 = 1.3% -> Grad Student 85
   north-campus-dorm: Jameson 4/300 = 1.3% -> Grad Student 85 *)
let rarest_by_prompt =
  [
    ("Name a Cornell all-you-care-to-eat dining hall", ("104 west", 60));
    ("Name a building on the Engineering Quad", ("bard", 85));
    ("Name a Cornell tradition", ("Senior Convocation", 60));
    ("Name a Cornell library", ("AFRICANA", 85));
    ("Name one of Cornell's undergraduate colleges or schools", ("Bowers CIS", 85));
    ("Name a residence hall on North Campus", ("  Jameson  ", 85));
  ]

(** No test game is longer than this, so a game that never ends fails the
    test instead of looping forever. *)
let max_steps = 50

(** [play s answer_for] answers every prompt of [s] with [answer_for prompt]
    and returns the final state. *)
let play s answer_for =
  let rec go s steps =
    if steps > max_steps then assert_failure "game never ended"
    else
      match Session.current_prompt s with
      | None -> s
      | Some p ->
          go (snd (Session.submit s (answer_for p.Terminal.text))) (steps + 1)
  in
  go s 0

(** [prompts_of s] is the text of every prompt [s] asks, in order, answering
    each one with nonsense. *)
let prompts_of s =
  let rec go s acc steps =
    if steps > max_steps then assert_failure "game never ended"
    else
      match Session.current_prompt s with
      | None -> List.rev acc
      | Some p ->
          go (snd (Session.submit s "zzz")) (p.Terminal.text :: acc) (steps + 1)
  in
  go s [] 0

let make_io inputs =
  let queue = ref inputs in
  let out = Buffer.create 256 in
  let io : Terminal.io =
    {
      print_line =
        (fun line ->
          Buffer.add_string out line;
          Buffer.add_char out '\n');
      read_line =
        (fun () ->
          match !queue with
          | [] -> None
          | x :: xs ->
              queue := xs;
              Some x);
    }
  in
  (io, fun () -> Buffer.contents out)

let has_substr haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec loop i = i + n <= h && (String.sub haystack i n = needle || loop (i + 1)) in
  loop 0

(** {1 Starting a game} *)

let test_start_first_prompt _ =
  let s = Session.start ~seed:42 in
  let expected = List.hd (Pack.select ~seed:42 ~count:5 (Pack.default ())) in
  match Session.current_prompt s with
  | Some p ->
      assert_equal 1 p.index;
      assert_equal 5 p.total;
      assert_equal ~printer:Fun.id expected.prompt p.text
  | None -> assert_failure "a new game should have a prompt"

let test_prompts_follow_pack_select _ =
  let expected =
    Pack.select ~seed:7 ~count:5 (Pack.default ())
    |> List.map (fun (q : Pack.question) -> q.prompt)
  in
  assert_equal expected (prompts_of (Session.start ~seed:7))

let test_no_repeated_prompts _ =
  let ps = prompts_of (Session.start ~seed:2026) in
  assert_equal ~printer:string_of_int 5 (List.length ps);
  assert_equal 5 (List.length (List.sort_uniq compare ps))

let test_same_seed_same_game _ =
  assert_equal
    (prompts_of (Session.start ~seed:3110))
    (prompts_of (Session.start ~seed:3110))

let test_seeds_change_game _ =
  let orders = List.map (fun seed -> prompts_of (Session.start ~seed)) [ 0; 1; 2; 3; 4 ] in
  assert_bool "five seeds should not all give the same game"
    (List.exists (fun o -> o <> List.hd orders) orders)

let test_rounds_capped_by_pack_size _ =
  let s = Session.start_with ~seed:0 ~rounds:10 test_pack in
  assert_equal ~printer:string_of_int 3 (List.length (prompts_of s))

let test_zero_rounds _ =
  let s = Session.start_with ~seed:0 ~rounds:0 test_pack in
  assert_bool "over at once" (Session.is_over s);
  assert_equal None (Session.current_prompt s);
  let r = Session.results s in
  assert_equal [] r.rounds;
  assert_equal 0 r.total_depth

let test_negative_rounds _ =
  assert_raises (Invalid_argument "Session.start_with: rounds must be >= 0")
    (fun () -> Session.start_with ~seed:0 ~rounds:(-1) test_pack)

(** {1 Matching} *)

let canonical_of q typed =
  Option.map (fun (a : Pack.answer) -> a.canonical) (Session.match_answer q typed)

let test_match_canonical_and_nickname _ =
  let q = question (Pack.default ()) "dining-hall" in
  assert_equal (Some "Okenshields") (canonical_of q "Okenshields");
  assert_equal (Some "Okenshields") (canonical_of q "okies");
  assert_equal (Some "104West!") (canonical_of q "104 West")

let test_match_case_spaces_apostrophes _ =
  let q = question (Pack.default ()) "tradition" in
  assert_equal (Some "Zinck's Night") (canonical_of q "  ZINCKS NIGHT ");
  assert_equal (Some "Slope Day") (canonical_of q "slope day")

let test_match_rejects _ =
  let lib = question (Pack.default ()) "library" in
  assert_equal None (canonical_of lib "");
  assert_equal None (canonical_of lib "Hogwarts");
  (* Olin is an Engineering Quad answer, but not a library: the library is
     "Olin Library" or "Olin Lib". *)
  assert_equal None (canonical_of lib "Olin");
  assert_equal (Some "Olin Library") (canonical_of lib "olin lib")

(** {1 Scoring} *)

let points q name = (Session.score q (answer q name)).points

let test_score_tiers _ =
  let fruit = question test_pack "fruit" in
  assert_equal ~printer:string_of_int 10 (points fruit "Apple");
  assert_equal ~printer:string_of_int 30 (points fruit "Cherry");
  assert_equal ~printer:string_of_int 60 (points fruit "Durian");
  assert_equal ~printer:string_of_int 85 (points fruit "Elderberry")

let test_score_boundaries _ =
  (* exactly 30% is Common; exactly 10% is Uncommon *)
  assert_equal ~printer:string_of_int 10 (points (question test_pack "fruit") "Banana");
  assert_equal ~printer:string_of_int 30 (points (question test_pack "color") "Blue")

let test_score_default_pack _ =
  let dp = Pack.default () in
  (* Okenshields 62/300 = 20.7%: the most popular hall, but under 30%. *)
  assert_equal ~printer:string_of_int 30 (points (question dp "dining-hall") "Okenshields");
  (* Slope Day 120/300 = 40% *)
  assert_equal ~printer:string_of_int 10 (points (question dp "tradition") "Slope Day");
  (* Olin Library 90/314 = 28.7% *)
  assert_equal ~printer:string_of_int 30 (points (question dp "library") "Olin Library")

(** {1 Submitting answers} *)

let test_submit_accepted _ =
  let s = Session.start_with ~seed:0 ~rounds:1 color_pack in
  let fb, s' = Session.submit s "  NAVY BLUE  " in
  assert_bool "accepted" fb.accepted;
  assert_equal ~printer:Fun.id "  NAVY BLUE  " fb.typed;
  assert_equal (Some "Blue") fb.understood_as;
  assert_equal ~printer:string_of_int 30 fb.points;
  assert_equal (Some "Freshman") fb.rarity_label;
  let r = Session.results s' in
  assert_equal 1 (List.length r.rounds);
  let round = List.hd r.rounds in
  assert_equal ~printer:Fun.id "Name a color" round.prompt_text;
  assert_equal (Some "Blue") round.understood_as;
  assert_equal 30 round.points

let test_submit_rejected_still_advances _ =
  let s = Session.start_with ~seed:0 ~rounds:1 color_pack in
  let fb, s' = Session.submit s "purple" in
  assert_bool "rejected" (not fb.accepted);
  assert_equal None fb.understood_as;
  assert_equal 0 fb.points;
  assert_equal None fb.rarity_label;
  assert_bool "game moves on after a miss" (Session.is_over s');
  assert_equal None (List.hd (Session.results s').rounds).understood_as

let test_submit_after_game_over _ =
  let s = Session.start_with ~seed:0 ~rounds:1 color_pack in
  let _, over = Session.submit s "red" in
  let fb, over' = Session.submit over "blue" in
  assert_bool "rejected" (not fb.accepted);
  assert_equal 0 fb.points;
  assert_equal (Session.results over) (Session.results over')

let test_old_state_unchanged _ =
  let s = Session.start_with ~seed:5 ~rounds:3 test_pack in
  let first = Session.current_prompt s in
  let _ = Session.submit s "anything" in
  assert_equal first (Session.current_prompt s);
  assert_equal [] (Session.results s).rounds

let test_results_mid_game _ =
  let s = Session.start_with ~seed:5 ~rounds:3 test_pack in
  let _, s1 = Session.submit s "zzz" in
  assert_bool "not over" (not (Session.is_over s1));
  assert_equal 1 (List.length (Session.results s1).rounds);
  match Session.current_prompt s1 with
  | Some p ->
      assert_equal 2 p.index;
      assert_equal 3 p.total
  | None -> assert_failure "two rounds should be left"

(** {1 Full games} *)

let test_full_game_test_pack _ =
  (* Elderberry 85 + Blue 30 + Pluto (not an answer) 0 = 115 points. *)
  let answer_for = function
    | "Name a fruit" -> "elderberry"
    | "Name a color" -> "Navy Blue"
    | _ -> "Pluto"
  in
  let s = play (Session.start_with ~seed:11 ~rounds:3 test_pack) answer_for in
  let r = Session.results s in
  assert_equal 11 r.seed;
  assert_equal 3 (List.length r.rounds);
  assert_equal ~printer:string_of_int 115 (Scoring.total_score r.rounds);
  assert_equal ~printer:string_of_int 1150 r.total_depth;
  let expected_order =
    Pack.select ~seed:11 ~count:3 test_pack
    |> List.map (fun (q : Pack.question) -> q.prompt)
  in
  assert_equal expected_order
    (List.map (fun (r : Terminal.round_result) -> r.prompt_text) r.rounds)

let test_full_game_default_pack_rarest _ =
  (* All six questions, each with its rarest answer:
     60 + 85 + 60 + 85 + 85 + 85 = 460 points = 4600 m. *)
  let answer_for p = fst (List.assoc p rarest_by_prompt) in
  let s = play (Session.start_with ~seed:1 ~rounds:6 (Pack.default ())) answer_for in
  let r = Session.results s in
  assert_equal 6 (List.length r.rounds);
  assert_bool "every answer accepted"
    (List.for_all (fun (r : Terminal.round_result) -> r.understood_as <> None) r.rounds);
  assert_equal ~printer:string_of_int 460 (Scoring.total_score r.rounds);
  assert_equal ~printer:string_of_int 4600 r.total_depth

let test_full_game_all_wrong _ =
  let s = play (Session.start ~seed:99) (fun _ -> "not a real answer") in
  let r = Session.results s in
  assert_equal 5 (List.length r.rounds);
  assert_equal 0 r.total_depth;
  assert_bool "every round a miss"
    (List.for_all (fun (r : Terminal.round_result) -> r.points = 0) r.rounds)

(** {1 Connected to the rest of the game} *)

let test_scripted_game_through_app _ =
  (* Same answers as the real player would type, in the order Pack.select
     gives for seed 7; expected points come from [rarest_by_prompt]. *)
  let qs = Pack.select ~seed:7 ~count:5 (Pack.default ()) in
  let typed = List.map (fun (q : Pack.question) -> fst (List.assoc q.prompt rarest_by_prompt)) qs in
  let expected_points =
    List.fold_left
      (fun acc (q : Pack.question) -> acc + snd (List.assoc q.prompt rarest_by_prompt))
      0 qs
  in
  let io, get_out = make_io typed in
  let r = App.play_with_driver io ~seed:7 (module Session) in
  let out = get_out () in
  assert_equal ~printer:string_of_int (expected_points * 10) r.total_depth;
  assert_bool "feedback shown" (has_substr out "Accepted");
  assert_bool "results shown"
    (has_substr out (Printf.sprintf "Total depth: %d" (expected_points * 10)))

let test_menu_play_uses_session _ =
  (* Play, seed 0, five wrong answers, then Quit. The temporary Driver's
     prompts are different, so seeing a real pack prompt shows Play runs a
     Session. *)
  let first = List.hd (Pack.select ~seed:0 ~count:5 (Pack.default ())) in
  let io, get_out = make_io [ "1"; "0"; "a"; "b"; "c"; "d"; "e"; "4" ] in
  App.run io;
  let out = get_out () in
  assert_bool "real pack prompt shown" (has_substr out first.prompt);
  assert_bool "results shown" (has_substr out "Total depth: 0");
  assert_bool "goodbye" (has_substr out "Goodbye!")

let test_results_feed_profile _ =
  (* Six accepted answers, none Common: Colin's Profile should count the game
     and unlock First Descent, Clean Run, and Off the Beaten Path, but not Gem
     Hunter (no Legend answers come from a pack). *)
  let answer_for p = fst (List.assoc p rarest_by_prompt) in
  let r = Session.results (play (Session.start_with ~seed:1 ~rounds:6 (Pack.default ())) answer_for) in
  let p, fresh = Profile.update Profile.empty r in
  assert_equal 1 p.games_played;
  assert_equal ~printer:string_of_int 460 p.high_score;
  assert_equal ~printer:string_of_int 4600 p.best_depth;
  assert_bool "first descent" (List.mem Profile.First_descent fresh);
  assert_bool "clean run" (List.mem Profile.Clean_run fresh);
  assert_bool "off the path" (List.mem Profile.Off_the_path fresh);
  assert_bool "no gem" (not (List.mem Profile.Gem_hunter fresh))

let suite =
  "session tests"
  >::: [
         "start_first_prompt" >:: test_start_first_prompt;
         "prompts_follow_pack_select" >:: test_prompts_follow_pack_select;
         "no_repeated_prompts" >:: test_no_repeated_prompts;
         "same_seed_same_game" >:: test_same_seed_same_game;
         "seeds_change_game" >:: test_seeds_change_game;
         "rounds_capped_by_pack_size" >:: test_rounds_capped_by_pack_size;
         "zero_rounds" >:: test_zero_rounds;
         "negative_rounds" >:: test_negative_rounds;
         "match_canonical_and_nickname" >:: test_match_canonical_and_nickname;
         "match_case_spaces_apostrophes" >:: test_match_case_spaces_apostrophes;
         "match_rejects" >:: test_match_rejects;
         "score_tiers" >:: test_score_tiers;
         "score_boundaries" >:: test_score_boundaries;
         "score_default_pack" >:: test_score_default_pack;
         "submit_accepted" >:: test_submit_accepted;
         "submit_rejected_still_advances" >:: test_submit_rejected_still_advances;
         "submit_after_game_over" >:: test_submit_after_game_over;
         "old_state_unchanged" >:: test_old_state_unchanged;
         "results_mid_game" >:: test_results_mid_game;
         "full_game_test_pack" >:: test_full_game_test_pack;
         "full_game_default_pack_rarest" >:: test_full_game_default_pack_rarest;
         "full_game_all_wrong" >:: test_full_game_all_wrong;
         "scripted_game_through_app" >:: test_scripted_game_through_app;
         "menu_play_uses_session" >:: test_menu_play_uses_session;
         "results_feed_profile" >:: test_results_feed_profile;
       ]

let () = run_test_tt_main suite
