(** OUnit tests for [Pack]: the default pack, parsing valid packs, rejecting
    malformed packs with the right line numbers, file loading, and seeded
    selection.

    {2 Ownership}
    - Primary: Gavin (gl568)
    - Backup: Karan (km2253)

    Expected values come from the pack format rules in [docs/PACK_FORMAT.md]
    and from the small fixture packs written out in each test, not from running
    the implementation. *)

open OUnit2
open Cornellion_Dune_Project

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec loop i = i + n <= h && (String.sub haystack i n = needle || loop (i + 1)) in
  n = 0 || loop 0

let parse_ok text =
  match Pack.of_string text with
  | Ok p -> p
  | Error e -> assert_failure ("expected a valid pack, got: " ^ Pack.error_to_string e)

(** [expect_error ~line ~msg text] checks that [text] is rejected, that the
    error is on [line], and that its message mentions [msg]. *)
let expect_error ?line ~msg text =
  match Pack.of_string text with
  | Ok _ -> assert_failure ("expected an error mentioning " ^ msg)
  | Error e ->
      assert_equal ~printer:(function
        | None -> "None"
        | Some n -> "Some " ^ string_of_int n)
        line e.line;
      assert_bool
        (Printf.sprintf "message %S should mention %S" e.message msg)
        (contains e.message msg)

let answer_names (q : Pack.question) =
  List.map (fun (a : Pack.answer) -> a.canonical) q.answers

let ids qs = List.map (fun (q : Pack.question) -> q.id) qs

(** {1 Default pack} *)

let test_default_loads _ =
  let p = Pack.default () in
  assert_equal ~printer:Fun.id "Cornell Basics" (Pack.name p);
  assert_equal ~printer:string_of_int 6 (Pack.size p);
  assert_equal
    [
      "dining-hall";
      "engineering-quad";
      "tradition";
      "library";
      "college";
      "north-campus-dorm";
    ]
    (ids (Pack.questions p))

let test_default_contents _ =
  let p = Pack.default () in
  match Pack.find p "dining-hall" with
  | None -> assert_failure "dining-hall missing"
  | Some q ->
      assert_bool "prompt" (contains q.prompt "dining hall");
      let okenshields = List.hd q.answers in
      assert_equal "Okenshields" okenshields.canonical;
      assert_equal 62 okenshields.count;
      assert_bool "Okies is a nickname" (List.mem "Okies" okenshields.aliases);
      assert_bool "104West! is an answer" (List.mem "104West!" (answer_names q))

(** Re-check the documented invariants on the shipped data, independently of
    the parser's own checks. *)
let test_default_invariants _ =
  let p = Pack.default () in
  let qs = Pack.questions p in
  assert_equal ~printer:string_of_int (List.length qs)
    (List.length (List.sort_uniq compare (ids qs)));
  List.iter
    (fun (q : Pack.question) ->
      assert_bool (q.id ^ " has a prompt") (q.prompt <> "");
      assert_bool (q.id ^ " has answers") (q.answers <> []);
      let names =
        List.concat_map
          (fun (a : Pack.answer) ->
            assert_bool (a.canonical ^ " count >= 1") (a.count >= 1);
            List.map String.lowercase_ascii (a.canonical :: a.aliases))
          q.answers
      in
      assert_equal ~msg:(q.id ^ " has no repeated names") (List.length names)
        (List.length (List.sort_uniq compare names));
      assert_equal (List.fold_left (fun s (a : Pack.answer) -> s + a.count) 0 q.answers)
        (Pack.total_count q))
    qs

(** {1 Parsing valid packs} *)

let small_pack =
  {|# A comment
pack:   Tiny Pack

question: dining-hall
prompt: Name a Cornell dining hall
answer: Okenshields | 180 | Okies,  Okenshield's
answer:   104West!   |   12

  # indented comment
question: eng-quad-2
prompt: Name a building on the Engineering Quad
answer: Upson Hall | 5 | upson hall, Upson, , UPSON
|}

let test_parse_small _ =
  let p = parse_ok small_pack in
  assert_equal ~printer:Fun.id "Tiny Pack" (Pack.name p);
  assert_equal ~printer:string_of_int 2 (Pack.size p);
  let dining = Option.get (Pack.find p "dining-hall") in
  assert_equal "Name a Cornell dining hall" dining.prompt;
  assert_equal [ "Okenshields"; "104West!" ] (answer_names dining);
  let ok = List.hd dining.answers in
  assert_equal ~printer:string_of_int 180 ok.count;
  assert_equal [ "Okies"; "Okenshield's" ] ok.aliases;
  let west = List.nth dining.answers 1 in
  assert_equal [] west.aliases;
  assert_equal ~printer:string_of_int 192 (Pack.total_count dining)

let test_parse_drops_self_repeats _ =
  (* "upson hall" repeats the name; "UPSON" repeats "Upson"; blank skipped. *)
  let p = parse_ok small_pack in
  let upson = List.hd (Option.get (Pack.find p "eng-quad-2")).answers in
  assert_equal [ "Upson" ] upson.aliases

let test_parse_untitled_and_crlf _ =
  let p =
    parse_ok "question: q1\r\nprompt: Say hi\r\nanswer: Hi | 1\r\n"
  in
  assert_equal ~printer:Fun.id "Untitled pack" (Pack.name p);
  assert_equal [ "Hi" ] (answer_names (List.hd (Pack.questions p)))

let test_find_missing _ =
  assert_equal None (Pack.find (parse_ok small_pack) "no-such-question")

(** {1 Rejecting malformed packs} *)

let q_header = "question: q\nprompt: P\n"

let test_error_no_colon _ =
  expect_error ~line:2 ~msg:"expected \"key: value\"" "question: q\nprompt P"

let test_error_unknown_key _ =
  expect_error ~line:3 ~msg:"unknown key \"answers\""
    (q_header ^ "answers: A | 1")

let test_error_before_question _ =
  expect_error ~line:1 ~msg:"prompt: appears before any question:" "prompt: P";
  expect_error ~line:2 ~msg:"answer: appears before any question:"
    "pack: X\nanswer: A | 1"

let test_error_missing_prompt _ =
  expect_error ~line:1 ~msg:"question \"q\" has no prompt"
    "question: q\nanswer: A | 1"

let test_error_no_answers _ =
  (* Reported at the question's line, when the next question starts... *)
  expect_error ~line:1 ~msg:"question \"q\" has no answers"
    "question: q\nprompt: P\nquestion: r\nprompt: R\nanswer: A | 1";
  (* ...or at the end of the file. *)
  expect_error ~line:4 ~msg:"question \"r\" has no answers"
    "question: q\nprompt: P\nanswer: A | 1\nquestion: r\nprompt: R"

let test_error_two_prompts _ =
  expect_error ~line:3 ~msg:"already has a prompt (line 2)"
    (q_header ^ "prompt: Again")

let test_error_bad_ids _ =
  expect_error ~line:1 ~msg:"lowercase letters, digits, and hyphens"
    "question: Dining Hall";
  expect_error ~line:4 ~msg:"duplicate question id \"q\" (first used on line 1)"
    (q_header ^ "answer: A | 1\nquestion: q")

let test_error_bad_counts _ =
  expect_error ~line:3 ~msg:"count \"lots\" is not a whole number"
    (q_header ^ "answer: A | lots");
  expect_error ~line:3 ~msg:"count must be at least 1" (q_header ^ "answer: A | 0");
  expect_error ~line:3 ~msg:"count must be at least 1" (q_header ^ "answer: A | -3")

let test_error_answer_shape _ =
  expect_error ~line:3 ~msg:"needs a name and a count" (q_header ^ "answer: A");
  expect_error ~line:3 ~msg:"too many | separators"
    (q_header ^ "answer: A | 1 | x | y");
  expect_error ~line:3 ~msg:"answer name is empty" (q_header ^ "answer:  | 1")

let test_error_clashes _ =
  (* A nickname that belongs to another answer, compared case-insensitively. *)
  expect_error ~line:4 ~msg:"\"BECKER\" is already used by answer \"Becker House\" on line 3"
    (q_header ^ "answer: Becker House | 3 | Becker\nanswer: Baker Hall | 2 | BECKER");
  (* Two answers with the same name. *)
  expect_error ~line:4 ~msg:"already used by answer"
    (q_header ^ "answer: Slope Day | 3\nanswer: slope  day | 2");
  (* The same nickname under different questions is fine. *)
  ignore
    (parse_ok
       (q_header ^ "answer: Risley Dining | 1 | Risley\n"
      ^ "question: r\nprompt: R\nanswer: Risley Hall | 1 | Risley"))

let test_error_pack_directive _ =
  expect_error ~line:2 ~msg:"more than once" "pack: A\npack: B";
  expect_error ~line:4 ~msg:"must come before the first question"
    (q_header ^ "answer: A | 1\npack: Late");
  expect_error ~line:1 ~msg:"pack name is empty" "pack:"

let test_error_empty _ =
  expect_error ?line:None ~msg:"pack has no questions" "";
  expect_error ?line:None ~msg:"pack has no questions" "# only a comment\npack: X\n"

let test_error_to_string _ =
  assert_equal ~printer:Fun.id "Line 7: bad"
    (Pack.error_to_string { line = Some 7; message = "bad" });
  assert_equal ~printer:Fun.id "bad"
    (Pack.error_to_string { line = None; message = "bad" })

(** {1 Files} *)

let test_load_file _ =
  let path = Filename.temp_file "cornellion_pack" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let oc = open_out path in
      output_string oc small_pack;
      close_out oc;
      match Pack.load_file path with
      | Ok p -> assert_equal ~printer:string_of_int 2 (Pack.size p)
      | Error e -> assert_failure (Pack.error_to_string e))

let test_load_missing_file _ =
  match Pack.load_file "/no/such/dir/pack.txt" with
  | Ok _ -> assert_failure "expected an error"
  | Error e ->
      assert_equal None e.line;
      assert_bool "mentions reading" (contains e.message "cannot read pack file")

(** {1 Selection} *)

let all_ids p = List.sort compare (ids (Pack.questions p))

let test_select_same_seed _ =
  let p = Pack.default () in
  let a = Pack.select ~seed:42 ~count:4 p in
  Random.self_init ();
  (* Must not depend on OCaml's global Random state. *)
  let b = Pack.select ~seed:42 ~count:4 p in
  assert_equal (ids a) (ids b)

let test_select_no_repeats _ =
  let p = Pack.default () in
  List.iter
    (fun seed ->
      let chosen = ids (Pack.select ~seed ~count:6 p) in
      (* Choosing every question gives each exactly once. *)
      assert_equal (all_ids p) (List.sort compare chosen))
    [ 0; 1; 2; 7; 42; 3110; -5; max_int; min_int ]

let test_select_counts _ =
  let p = Pack.default () in
  assert_equal [] (Pack.select ~seed:1 ~count:0 p);
  assert_equal ~printer:string_of_int 3
    (List.length (Pack.select ~seed:1 ~count:3 p));
  (* Asking for more than the pack has returns every question once. *)
  assert_equal (all_ids p) (List.sort compare (ids (Pack.select ~seed:1 ~count:50 p)));
  assert_raises (Invalid_argument "Pack.select: count must be non-negative")
    (fun () -> Pack.select ~seed:1 ~count:(-1) p)

let test_select_prefix _ =
  (* A shorter game with the same seed asks the first questions of the longer
     one, so the session can stop early without changing earlier rounds. *)
  let p = Pack.default () in
  let long = ids (Pack.select ~seed:9 ~count:6 p) in
  let short = ids (Pack.select ~seed:9 ~count:2 p) in
  assert_equal short [ List.nth long 0; List.nth long 1 ]

let test_select_varies_with_seed _ =
  (* Over many seeds, every question should be chosen first at least once. *)
  let p = Pack.default () in
  let firsts =
    List.init 100 (fun seed -> (List.hd (Pack.select ~seed ~count:1 p)).Pack.id)
    |> List.sort_uniq compare
  in
  assert_equal (all_ids p) firsts

let suite =
  "pack"
  >::: [
         "default_loads" >:: test_default_loads;
         "default_contents" >:: test_default_contents;
         "default_invariants" >:: test_default_invariants;
         "parse_small" >:: test_parse_small;
         "parse_drops_self_repeats" >:: test_parse_drops_self_repeats;
         "parse_untitled_and_crlf" >:: test_parse_untitled_and_crlf;
         "find_missing" >:: test_find_missing;
         "error_no_colon" >:: test_error_no_colon;
         "error_unknown_key" >:: test_error_unknown_key;
         "error_before_question" >:: test_error_before_question;
         "error_missing_prompt" >:: test_error_missing_prompt;
         "error_no_answers" >:: test_error_no_answers;
         "error_two_prompts" >:: test_error_two_prompts;
         "error_bad_ids" >:: test_error_bad_ids;
         "error_bad_counts" >:: test_error_bad_counts;
         "error_answer_shape" >:: test_error_answer_shape;
         "error_clashes" >:: test_error_clashes;
         "error_pack_directive" >:: test_error_pack_directive;
         "error_empty" >:: test_error_empty;
         "error_to_string" >:: test_error_to_string;
         "load_file" >:: test_load_file;
         "load_missing_file" >:: test_load_missing_file;
         "select_same_seed" >:: test_select_same_seed;
         "select_no_repeats" >:: test_select_no_repeats;
         "select_counts" >:: test_select_counts;
         "select_prefix" >:: test_select_prefix;
         "select_varies_with_seed" >:: test_select_varies_with_seed;
       ]

let () = run_test_tt_main suite
