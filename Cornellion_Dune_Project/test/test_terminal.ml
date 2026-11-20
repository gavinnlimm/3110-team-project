(** OUnit tests for Terminal screens and a scripted MVP game. *)

open OUnit2
open Cornellion_Dune_Project

let has_substr haystack needle =
  let n = String.length needle in
  let h = String.length haystack in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i + n > h then false
      else if String.sub haystack i n = needle then true
      else loop (i + 1)
    in
    loop 0

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

let prompt_sample : Terminal.prompt_view =
  { index = 1; total = 3; text = "Name a Cornell dining hall" }

let feedback_accepted : Terminal.feedback_view =
  {
    accepted = true;
    typed = "okies";
    understood_as = Some "Okenshields";
    points = 5;
    rarity_label = Some "Common";
  }

let feedback_rejected : Terminal.feedback_view =
  {
    accepted = false;
    typed = "not a place";
    understood_as = None;
    points = 0;
    rarity_label = None;
  }

let results_sample : Terminal.game_results =
  {
    seed = 42;
    rounds =
      [
        {
          prompt_text = "Name a Cornell dining hall";
          typed = "okies";
          understood_as = Some "Okenshields";
          points = 5;
          rarity_label = Some "Common";
        };
        {
          prompt_text = "Name a building on the Engineering Quad";
          typed = "Upson";
          understood_as = Some "Upson Hall";
          points = 10;
          rarity_label = Some "Common";
        };
      ];
    total_depth = 15;
  }

let stats_sample : Terminal.stats_view =
  {
    games_played = 2;
    best_depth = 40;
    average_depth = 27.5;
    note = Some "Sample note";
  }

let test_render_main_menu _ =
  let s = Terminal.render_main_menu () in
  assert_bool "title" (has_substr s "Cornell Krillion");
  assert_bool "play" (has_substr s "Play");
  assert_bool "stats" (has_substr s "Statistics");
  assert_bool "bank" (has_substr s "Question bank");
  assert_bool "quit" (has_substr s "Quit")

let test_render_bank_menu _ =
  let s = Terminal.render_bank_menu () in
  assert_bool "default" (has_substr s "Default bank");
  assert_bool "custom" (has_substr s "Custom bank")

let test_render_prompt _ =
  let s = Terminal.render_prompt prompt_sample in
  assert_bool "round" (has_substr s "Round 1 of 3");
  assert_bool "text" (has_substr s "Name a Cornell dining hall");
  assert_bool "answer prompt" (has_substr s "Your answer:")

let test_render_feedback_accepted _ =
  let s = Terminal.render_feedback feedback_accepted in
  assert_bool "accepted" (has_substr s "Accepted");
  assert_bool "canonical" (has_substr s "Okenshields");
  assert_bool "points" (has_substr s "Points: 5");
  assert_bool "rarity" (has_substr s "Common")

let test_render_feedback_rejected _ =
  let s = Terminal.render_feedback feedback_rejected in
  assert_bool "rejected" (has_substr s "Not accepted");
  assert_bool "typed" (has_substr s "not a place");
  assert_bool "zero" (has_substr s "Points: 0")

let test_render_results _ =
  let s = Terminal.render_results results_sample in
  assert_bool "title" (has_substr s "Results");
  assert_bool "seed" (has_substr s "Seed: 42");
  assert_bool "depth" (has_substr s "Total depth: 15");
  assert_bool "matched" (has_substr s "Okenshields")

let test_render_stats _ =
  let s = Terminal.render_stats stats_sample in
  assert_bool "title" (has_substr s "Statistics");
  assert_bool "games" (has_substr s "Games played: 2");
  assert_bool "best" (has_substr s "Best depth:   40");
  assert_bool "avg" (has_substr s "27.50");
  assert_bool "note" (has_substr s "Sample note")

let test_parse_menu_choice _ =
  assert_equal (Some Terminal.Play) (Terminal.parse_menu_choice "1");
  assert_equal (Some Terminal.Play) (Terminal.parse_menu_choice " play ");
  assert_equal (Some Terminal.Stats) (Terminal.parse_menu_choice "2");
  assert_equal (Some Terminal.Bank) (Terminal.parse_menu_choice "Bank");
  assert_equal (Some Terminal.Quit) (Terminal.parse_menu_choice "quit");
  assert_equal None (Terminal.parse_menu_choice "nope")

let test_read_menu_invalid_then_valid _ =
  let io, get_out = make_io [ "nope"; "1" ] in
  let choice = Terminal.read_menu_choice io in
  assert_equal Terminal.Play choice;
  assert_bool "retry message" (has_substr (get_out ()) "Invalid choice")

let test_full_scripted_game _ =
  (* seed 0 → builtin order: dining, eng quad, tradition *)
  let answers = [ "Okies"; "Upson Hall"; "Dragon Day" ] in
  let io, get_out = make_io answers in
  let results =
    App.play_with_driver io ~seed:0 (module Mvp_driver.Mvp_driver)
  in
  let out = get_out () in
  assert_equal 3 (List.length results.Terminal.rounds);
  assert_equal 5 (List.nth results.rounds 0).points;
  assert_equal 10 (List.nth results.rounds 1).points;
  assert_equal 14 (List.nth results.rounds 2).points;
  assert_equal 29 results.total_depth;
  assert_equal (Some "Okenshields") (List.nth results.rounds 0).understood_as;
  assert_equal (Some "Upson Hall") (List.nth results.rounds 1).understood_as;
  assert_equal (Some "Dragon Day") (List.nth results.rounds 2).understood_as;
  assert_bool "feedback shown" (has_substr out "Accepted");
  assert_bool "results shown" (has_substr out "Total depth: 29")

let test_rejected_answer_path _ =
  let io, get_out = make_io [ "totally fake"; "zzz"; "still wrong" ] in
  let results =
    App.play_with_driver io ~seed:0 (module Mvp_driver.Mvp_driver)
  in
  let out = get_out () in
  assert_equal 0 results.total_depth;
  List.iter
    (fun (r : Terminal.round_result) ->
      assert_equal None r.understood_as;
      assert_equal 0 r.points)
    results.rounds;
  assert_bool "rejection shown" (has_substr out "Not accepted")

let test_seed_rotates_prompt_order _ =
  let state0 = Mvp_driver.Mvp_driver.start ~seed:0 in
  let state1 = Mvp_driver.Mvp_driver.start ~seed:1 in
  let p0 = Mvp_driver.Mvp_driver.current_prompt state0 in
  let p1 = Mvp_driver.Mvp_driver.current_prompt state1 in
  match (p0, p1) with
  | Some a, Some b ->
      assert_bool "seed changes first prompt" (a.text <> b.text)
  | _ -> assert_failure "expected prompts"

let test_app_quit _ =
  let io, get_out = make_io [ "4" ] in
  App.run io;
  assert_bool "goodbye" (has_substr (get_out ()) "Goodbye!")

let test_app_stats_then_quit _ =
  let io, get_out = make_io [ "2"; "4" ] in
  App.run io;
  let out = get_out () in
  assert_bool "stats" (has_substr out "Statistics");
  assert_bool "placeholder" (has_substr out "Stats will populate");
  assert_bool "goodbye" (has_substr out "Goodbye!")

let suite =
  "terminal"
  >::: [
         "render_main_menu" >:: test_render_main_menu;
         "render_bank_menu" >:: test_render_bank_menu;
         "render_prompt" >:: test_render_prompt;
         "render_feedback_accepted" >:: test_render_feedback_accepted;
         "render_feedback_rejected" >:: test_render_feedback_rejected;
         "render_results" >:: test_render_results;
         "render_stats" >:: test_render_stats;
         "parse_menu_choice" >:: test_parse_menu_choice;
         "read_menu_invalid_then_valid" >:: test_read_menu_invalid_then_valid;
         "full_scripted_game" >:: test_full_scripted_game;
         "rejected_answer_path" >:: test_rejected_answer_path;
         "seed_rotates_prompt_order" >:: test_seed_rotates_prompt_order;
         "app_quit" >:: test_app_quit;
         "app_stats_then_quit" >:: test_app_stats_then_quit;
       ]

let () = run_test_tt_main suite
