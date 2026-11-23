open OUnit2
open Cornellion_Dune_Project

let prompt : Pack.prompt_entry =
  {
    text = "Name a Cornell dining hall";
    answers =
      [
        {
          canonical = "Okenshields";
          aliases = [ "Okies" ];
          points = 5;
          rarity_label = "Common";
        };
      ];
  }

let test_valid _ =
  match Matcher.find_answer prompt "Okenshields" with
  | Some a -> assert_equal "Okenshields" a.canonical
  | None -> assert_failure "Valid answer was rejected"

let test_invalid _ = assert_equal None (Matcher.find_answer prompt "McDonalds")

let test_capitalization _ =
  match Matcher.find_answer prompt "oKeNsHiElDs" with
  | Some a -> assert_equal "Okenshields" a.canonical
  | None -> assert_failure "Case-insensitive match failed"

let test_whitespace _ =
  assert_bool "Extra whitespace should be ignored"
    (Matcher.matches_alias "  UPSON   Hall  " "Upson Hall")

let test_alias _ =
  match Matcher.find_answer prompt "okies" with
  | Some a -> assert_equal "Okenshields" a.canonical
  | None -> assert_failure "Alias match failed"

let test_whitespace_answer _ =
  match Matcher.find_answer prompt "  Okenshields  " with
  | Some a -> assert_equal "Okenshields" a.canonical
  | None -> assert_failure "Whitespace matching failed"

let test_empty _ = assert_equal None (Matcher.find_answer prompt "")

let test_punctuation _ =
  assert_bool "Punctuation should be ignored"
    (Matcher.matches_alias "O'kenshields" "Okenshields")

let suite =
  "matcher"
  >::: [
         "valid" >:: test_valid;
         "invalid" >:: test_invalid;
         "capitalization" >:: test_capitalization;
         "whitespace" >:: test_whitespace;
         "alias" >:: test_alias;
         "whitespace_answer" >:: test_whitespace_answer;
         "empty" >:: test_empty;
         "punctuation" >:: test_punctuation;
       ]

let () = run_test_tt_main suite
