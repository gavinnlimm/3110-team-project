(** Question packs. See [pack.mli] for the interface and invariants, and
    [docs/PACK_FORMAT.md] for the file format.

    The parser reads one line at a time and keeps the question it is currently
    building in [state.current]. A question is checked and moved to the
    finished list when the next [question:] line or the end of the file is
    reached. All errors are raised as [Parse_error] inside this module and
    turned into [Error] by [of_string], so no exception escapes. *)

type answer = {
  canonical : string;
  aliases : string list;
  count : int;
}

type question = {
  id : string;
  prompt : string;
  answers : answer list;
}

type t = {
  pack_name : string;
  qs : question list;  (** file order, never empty *)
}

type error = {
  line : int option;
  message : string;
}

exception Parse_error of error

let fail line message = raise (Parse_error { line = Some line; message })

let error_to_string e =
  match e.line with
  | Some n -> Printf.sprintf "Line %d: %s" n e.message
  | None -> e.message

(** {1 Helpers} *)

(** Key used to detect two answers that a player could not tell apart:
    lowercase, trimmed, with runs of spaces collapsed to one. *)
let clash_key s =
  String.lowercase_ascii s
  |> String.split_on_char ' '
  |> List.filter (fun w -> w <> "")
  |> String.concat " "

let valid_id id =
  id <> ""
  && String.for_all
       (fun c ->
         match c with
         | 'a' .. 'z'
         | '0' .. '9'
         | '-' ->
             true
         | _ -> false)
       id

(** [split_directive s] is [Some (key, value)] for a line ["key: value"]. *)
let split_directive s =
  match String.index_opt s ':' with
  | None -> None
  | Some i ->
      let key = String.trim (String.sub s 0 i) in
      let value = String.trim (String.sub s (i + 1) (String.length s - i - 1)) in
      Some (key, value)

(** {1 Parser state} *)

(** The question being built, with the line numbers needed for messages. *)
type draft = {
  d_id : string;
  d_line : int;  (** line of the [question:] directive *)
  d_prompt : string option;
  d_prompt_line : int;
  d_answers : answer list;  (** reversed *)
  d_names : (string * (string * int)) list;
      (** [clash_key] of every name and nickname used so far, mapped to the
          owning answer's name and line *)
}

type state = {
  name : string option;
  done_qs : question list;  (** reversed *)
  ids : (string * int) list;  (** question id -> line it was defined on *)
  current : draft option;
}

let empty_state = { name = None; done_qs = []; ids = []; current = None }

(** Check a finished draft and turn it into a question. *)
let finish_draft d =
  match d.d_prompt with
  | None -> fail d.d_line (Printf.sprintf "question %S has no prompt" d.d_id)
  | Some prompt ->
      if d.d_answers = [] then
        fail d.d_line (Printf.sprintf "question %S has no answers" d.d_id)
      else { id = d.d_id; prompt; answers = List.rev d.d_answers }

let close_current st =
  match st.current with
  | None -> st
  | Some d -> { st with done_qs = finish_draft d :: st.done_qs; current = None }

(** {1 Directives} *)

let parse_count line s =
  match int_of_string_opt s with
  | None -> fail line (Printf.sprintf "count %S is not a whole number" s)
  | Some n when n < 1 ->
      fail line (Printf.sprintf "count must be at least 1, but is %d" n)
  | Some n -> n

(** Parse the value of an [answer:] line and add it to draft [d]. *)
let add_answer line d value =
  let fields = List.map String.trim (String.split_on_char '|' value) in
  let canonical, count_s, alias_s =
    match fields with
    | [ c; n ] -> (c, n, "")
    | [ c; n; a ] -> (c, n, a)
    | [ _ ] ->
        fail line
          "answer needs a name and a count, like: answer: Okenshields | 62 | \
           Okies"
    | _ -> fail line "answer has too many | separators (expected at most 2)"
  in
  if canonical = "" then fail line "answer name is empty";
  let count = parse_count line count_s in
  let canon_key = clash_key canonical in
  (* Nicknames: skip blanks and repeats of this answer's own names. *)
  let aliases =
    String.split_on_char ',' alias_s
    |> List.map String.trim
    |> List.filter (fun a -> a <> "")
    |> List.fold_left
         (fun acc a ->
           let k = clash_key a in
           if k = canon_key || List.exists (fun b -> clash_key b = k) acc then
             acc
           else a :: acc)
         []
    |> List.rev
  in
  (* No name or nickname may already belong to another answer. *)
  let names = canonical :: aliases in
  List.iter
    (fun n ->
      match List.assoc_opt (clash_key n) d.d_names with
      | Some (owner, owner_line) ->
          fail line
            (Printf.sprintf "%S is already used by answer %S on line %d" n owner
               owner_line)
      | None -> ())
    names;
  let new_names = List.map (fun n -> (clash_key n, (canonical, line))) names in
  {
    d with
    d_answers = { canonical; aliases; count } :: d.d_answers;
    d_names = new_names @ d.d_names;
  }

let handle_directive st line key value =
  match key with
  | "pack" ->
      if st.name <> None then fail line "pack name is given more than once"
      else if st.current <> None || st.done_qs <> [] then
        fail line "pack: must come before the first question"
      else if value = "" then fail line "pack name is empty"
      else { st with name = Some value }
  | "question" ->
      if not (valid_id value) then
        fail line
          (Printf.sprintf
             "question id %S must use only lowercase letters, digits, and \
              hyphens"
             value);
      (match List.assoc_opt value st.ids with
      | Some first ->
          fail line
            (Printf.sprintf "duplicate question id %S (first used on line %d)"
               value first)
      | None -> ());
      let st = close_current st in
      {
        st with
        ids = (value, line) :: st.ids;
        current =
          Some
            {
              d_id = value;
              d_line = line;
              d_prompt = None;
              d_prompt_line = 0;
              d_answers = [];
              d_names = [];
            };
      }
  | "prompt" -> (
      match st.current with
      | None -> fail line "prompt: appears before any question:"
      | Some d -> (
          match d.d_prompt with
          | Some _ ->
              fail line
                (Printf.sprintf "question %S already has a prompt (line %d)"
                   d.d_id d.d_prompt_line)
          | None ->
              if value = "" then fail line "prompt is empty"
              else
                {
                  st with
                  current =
                    Some { d with d_prompt = Some value; d_prompt_line = line };
                }))
  | "answer" -> (
      match st.current with
      | None -> fail line "answer: appears before any question:"
      | Some d -> { st with current = Some (add_answer line d value) })
  | other ->
      fail line
        (Printf.sprintf
           "unknown key %S (expected pack, question, prompt, or answer)" other)

let handle_line st (line, raw) =
  let s = String.trim raw in
  if s = "" || s.[0] = '#' then st
  else
    match split_directive s with
    | None -> fail line (Printf.sprintf "expected \"key: value\" but got %S" s)
    | Some (key, value) -> handle_directive st line key value

(** {1 Loading} *)

let of_string text =
  try
    let lines =
      String.split_on_char '\n' text
      |> List.mapi (fun i l ->
             (* Accept Windows line endings. *)
             let l =
               if String.length l > 0 && l.[String.length l - 1] = '\r' then
                 String.sub l 0 (String.length l - 1)
               else l
             in
             (i + 1, l))
    in
    let st = List.fold_left handle_line empty_state lines |> close_current in
    match List.rev st.done_qs with
    | [] -> Error { line = None; message = "pack has no questions" }
    | qs ->
        Ok
          {
            pack_name = Option.value st.name ~default:"Untitled pack";
            qs;
          }
  with Parse_error e -> Error e

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let load_file path =
  match read_file path with
  | exception Sys_error msg ->
      Error { line = None; message = "cannot read pack file: " ^ msg }
  | text -> of_string text

let default () =
  match of_string Default_pack_data.text with
  | Ok p -> p
  | Error e ->
      failwith ("built-in default pack is malformed: " ^ error_to_string e)

(** {1 Queries} *)

let name p = p.pack_name
let questions p = p.qs
let size p = List.length p.qs
let find p id = List.find_opt (fun q -> q.id = id) p.qs
let total_count q = List.fold_left (fun acc a -> acc + a.count) 0 q.answers

(** {1 Selection} *)

(** A tiny linear congruential generator. We use our own instead of [Random]
    because [Random]'s sequence for a given seed changed between OCaml 4 and
    OCaml 5, and a shared seed must give every teammate the same game. The
    state stays below 2{^31}, so the multiplication cannot overflow a 63-bit
    OCaml int. *)
let next_state s = ((s * 1103515245) + 12345) land 0x7FFFFFFF

let select ~seed ~count p =
  if count < 0 then invalid_arg "Pack.select: count must be non-negative";
  let arr = Array.of_list p.qs in
  let n = Array.length arr in
  (* Fisher-Yates shuffle driven by the generator above. *)
  let state = ref (next_state (seed land 0x7FFFFFFF)) in
  for i = n - 1 downto 1 do
    state := next_state !state;
    let j = (!state lsr 8) mod (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.to_list (Array.sub arr 0 (min count n))

(** Temporary demo bank preserved from Driver. *)
type answer_entry = {
  canonical : string;
  aliases : string list;
  points : int;
  rarity_label : string;
}

type prompt_entry = {
  text : string;
  answers : answer_entry list;
}

(** Small built-in pack for demos and UI tests only (Gavin will replace). *)
let builtin_prompts : prompt_entry list =
  [
    {
      text = "Name a Cornell dining hall";
      answers =
        [
          {
            canonical = "Okenshields";
            aliases = [ "okenshields"; "okies"; "okenshield's" ];
            points = 5;
            rarity_label = "Common";
          };
          {
            canonical = "104West!";
            aliases = [ "104west!"; "104west"; "104 west"; "104 west!" ];
            points = 25;
            rarity_label = "Rare";
          };
          {
            canonical = "Becker House";
            aliases = [ "becker"; "becker house" ];
            points = 15;
            rarity_label = "Uncommon";
          };
        ];
    };
    {
      text = "Name a building on the Engineering Quad";
      answers =
        [
          {
            canonical = "Phillips Hall";
            aliases = [ "phillips"; "phillips hall" ];
            points = 8;
            rarity_label = "Common";
          };
          {
            canonical = "Upson Hall";
            aliases = [ "upson"; "upson hall" ];
            points = 10;
            rarity_label = "Common";
          };
          {
            canonical = "Duffield Hall";
            aliases = [ "duffield"; "duffield hall" ];
            points = 12;
            rarity_label = "Uncommon";
          };
        ];
    };
    {
      text = "Name a Cornell tradition";
      answers =
        [
          {
            canonical = "Slope Day";
            aliases = [ "slope day"; "slopeday" ];
            points = 6;
            rarity_label = "Common";
          };
          {
            canonical = "Dragon Day";
            aliases = [ "dragon day"; "dragonday" ];
            points = 14;
            rarity_label = "Uncommon";
          };
          {
            canonical = "Hockey Peanuts";
            aliases = [ "hockey peanuts"; "peanuts" ];
            points = 22;
            rarity_label = "Rare";
          };
        ];
    };
  ]

(** Deterministic rotation of prompts from [seed] (Karan will replace). *)
let order_prompts seed =
  let n = List.length builtin_prompts in
  let offset = if n = 0 then 0 else abs seed mod n in
  let rec rotate k lst =
    if k = 0 then lst
    else
      match lst with
      | [] -> []
      | h :: t -> rotate (k - 1) (t @ [ h ])
  in
  rotate offset builtin_prompts