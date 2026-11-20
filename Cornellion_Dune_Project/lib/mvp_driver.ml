(** Temporary MVP game driver for terminal integration. *)

module type Game_driver = sig
  type t

  val start : seed:int -> t
  val current_prompt : t -> Terminal.prompt_view option
  val submit : t -> string -> Terminal.feedback_view * t
  val results : t -> Terminal.game_results
end

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

(** Small built-in pack for demos and UI tests only. *)
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

let normalize s =
  let trimmed = String.trim s |> String.lowercase_ascii in
  let buf = Buffer.create (String.length trimmed) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z'
      | '0' .. '9'
      | ' ' ->
          Buffer.add_char buf c
      | '\'' -> ()
      | _ -> ())
    trimmed;
  Buffer.contents buf |> String.trim

let matches_alias typed alias = normalize typed = normalize alias

let find_answer (prompt : prompt_entry) typed =
  let rec search answers =
    match answers with
    | [] -> None
    | a :: rest ->
        if
          matches_alias typed a.canonical
          || List.exists (matches_alias typed) a.aliases
        then Some a
        else search rest
  in
  search prompt.answers

(** Deterministic rotation of prompts from [seed]. *)
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

module Mvp_driver = struct
  type t = {
    seed : int;
    prompts : prompt_entry list;
    index : int;  (** next prompt index to show *)
    rounds : Terminal.round_result list;  (** completed, oldest first *)
  }

  let start ~seed =
    { seed; prompts = order_prompts seed; index = 0; rounds = [] }

  let current_prompt (state : t) =
    let total = List.length state.prompts in
    if state.index >= total then None
    else
      let prompt = List.nth state.prompts state.index in
      Some
        {
          Terminal.index = state.index + 1;
          total;
          text = prompt.text;
        }

  let submit (state : t) typed =
    let total = List.length state.prompts in
    if state.index >= total then
      let feedback =
        {
          Terminal.accepted = false;
          typed;
          understood_as = None;
          points = 0;
          rarity_label = None;
        }
      in
      (feedback, state)
    else
      let prompt = List.nth state.prompts state.index in
      let feedback, round =
        match find_answer prompt typed with
        | Some a ->
            ( {
                Terminal.accepted = true;
                typed;
                understood_as = Some a.canonical;
                points = a.points;
                rarity_label = Some a.rarity_label;
              },
              {
                Terminal.prompt_text = prompt.text;
                typed;
                understood_as = Some a.canonical;
                points = a.points;
                rarity_label = Some a.rarity_label;
              } )
        | None ->
            ( {
                Terminal.accepted = false;
                typed;
                understood_as = None;
                points = 0;
                rarity_label = None;
              },
              {
                Terminal.prompt_text = prompt.text;
                typed;
                understood_as = None;
                points = 0;
                rarity_label = None;
              } )
      in
      let state' =
        {
          state with
          index = state.index + 1;
          rounds = state.rounds @ [ round ];
        }
      in
      (feedback, state')

  let results (state : t) =
    let total_depth =
      List.fold_left (fun acc (r : Terminal.round_result) -> acc + r.points) 0
        state.rounds
    in
    { Terminal.seed = state.seed; rounds = state.rounds; total_depth }
end

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
