(** Terminal screens and scriptable I/O for Cornell Krillion.

    See [terminal.mli] and [docs/FUTURE_WORK.md] for ownership and remaining
    work. Do not add pack loading, matching, scoring, or session state here—
    map teammate domain types into the view models and call the renderers. *)

type prompt_view = {
  index : int;
  total : int;
  text : string;
}

type feedback_view = {
  accepted : bool;
  typed : string;
  understood_as : string option;
  points : int;
  rarity_label : string option;
}

type round_result = {
  prompt_text : string;
  typed : string;
  understood_as : string option;
  points : int;
  rarity_label : string option;
}

type game_results = {
  seed : int;
  rounds : round_result list;
  total_depth : int;
}

type stats_view = {
  games_played : int;
  best_depth : int;
  average_depth : float;
  note : string option;
}

type menu_choice =
  | Play
  | Stats
  | Bank
  | Quit

type bank_choice =
  | Default_bank
  | Custom_bank
  | Back

type io = {
  print_line : string -> unit;
  read_line : unit -> string option;
}

let stdio =
  {
    print_line = print_endline;
    read_line =
      (fun () ->
        try Some (read_line ()) with
        | End_of_file -> None);
  }

let render_main_menu () =
  String.concat "\n"
    [
      "========================================";
      "         Cornell Krillion";
      "========================================";
      "";
      "  1) Play";
      "  2) Statistics";
      "  3) Question bank";
      "  4) Quit";
      "";
      "Enter a choice:";
    ]

let render_bank_menu () =
  String.concat "\n"
    [
      "----------------------------------------";
      "         Question bank";
      "----------------------------------------";
      "";
      "  1) Default bank";
      "  2) Custom bank (coming soon)";
      "  3) Back";
      "";
      "Enter a choice:";
    ]

let render_prompt (p : prompt_view) =
  String.concat "\n"
    [
      "----------------------------------------";
      Printf.sprintf "  Round %d of %d" p.index p.total;
      "----------------------------------------";
      "";
      p.text;
      "";
      "Your answer:";
    ]

let render_feedback (f : feedback_view) =
  if f.accepted then
    let understood =
      match f.understood_as with
      | Some s -> s
      | None -> f.typed
    in
    let rarity =
      match f.rarity_label with
      | Some r -> r
      | None -> "Unknown"
    in
    String.concat "\n"
      [
        "";
        "  Accepted!";
        Printf.sprintf "  Understood as: %s" understood;
        Printf.sprintf "  Rarity: %s" rarity;
        Printf.sprintf "  Points: %d" f.points;
        "";
      ]
  else
    String.concat "\n"
      [
        "";
        "  Not accepted.";
        Printf.sprintf "  You typed: %s" f.typed;
        "  Points: 0";
        "";
      ]

let render_results (g : game_results) =
  let header =
    [
      "========================================";
      "              Results";
      "========================================";
      Printf.sprintf "  Seed: %d" g.seed;
      "";
    ]
  in
  let round_lines =
    List.mapi
      (fun i (r : round_result) ->
        let understood =
          match r.understood_as with
          | Some s -> s
          | None -> "(none)"
        in
        let rarity =
          match r.rarity_label with
          | Some s -> s
          | None -> "—"
        in
        [
          Printf.sprintf "  Round %d: %s" (i + 1) r.prompt_text;
          Printf.sprintf "    Typed: %s" r.typed;
          Printf.sprintf "    Matched: %s" understood;
          Printf.sprintf "    Rarity: %s  Points: %d" rarity r.points;
          "";
        ])
      g.rounds
    |> List.flatten
  in
  let footer =
    [
      Printf.sprintf "  Total depth: %d" g.total_depth;
      "========================================";
    ]
  in
  String.concat "\n" (header @ round_lines @ footer)

let render_stats (s : stats_view) =
  let note_lines =
    match s.note with
    | Some n -> [ ""; "  " ^ n ]
    | None -> []
  in
  String.concat "\n"
    ([
       "========================================";
       "            Statistics";
       "========================================";
       "";
       Printf.sprintf "  Games played: %d" s.games_played;
       Printf.sprintf "  Best depth:   %d" s.best_depth;
       Printf.sprintf "  Average depth: %.2f" s.average_depth;
     ]
    @ note_lines
    @ [ ""; "========================================" ])

let print_screen (io : io) screen =
  let lines = String.split_on_char '\n' screen in
  List.iter io.print_line lines;
  io.print_line ""

let normalize_token s =
  String.lowercase_ascii (String.trim s)

let parse_menu_choice raw =
  match normalize_token raw with
  | "1" | "play" | "p" -> Some Play
  | "2" | "stats" | "statistics" | "s" -> Some Stats
  | "3" | "bank" | "question bank" | "q" -> Some Bank
  | "4" | "quit" | "exit" | "x" -> Some Quit
  | _ -> None

let parse_bank_choice raw =
  match normalize_token raw with
  | "1" | "default" | "d" -> Some Default_bank
  | "2" | "custom" | "c" -> Some Custom_bank
  | "3" | "back" | "b" -> Some Back
  | _ -> None

let rec read_menu_choice (io : io) =
  print_screen io (render_main_menu ());
  match io.read_line () with
  | None -> Quit
  | Some line -> (
      match parse_menu_choice line with
      | Some choice -> choice
      | None ->
          io.print_line "Invalid choice. Please try again.";
          io.print_line "";
          read_menu_choice io)

let rec read_bank_choice (io : io) =
  print_screen io (render_bank_menu ());
  match io.read_line () with
  | None -> Back
  | Some line -> (
      match parse_bank_choice line with
      | Some choice -> choice
      | None ->
          io.print_line "Invalid choice. Please try again.";
          io.print_line "";
          read_bank_choice io)

let read_answer (io : io) = io.read_line ()

let read_seed (io : io) =
  io.print_line "Enter a game seed (blank = 0):";
  match io.read_line () with
  | None -> 0
  | Some line -> (
      let trimmed = String.trim line in
      if trimmed = "" then 0
      else
        try int_of_string trimmed with
        | Failure _ ->
            io.print_line "Could not parse seed; using 0.";
            0)

module type Session = sig
  type t

  val current_prompt : t -> prompt_view option
  val submit : t -> string -> feedback_view * t
  val results : t -> game_results
end

let run_play_session (type a) (io : io) (module S : Session with type t = a)
    (session : a) =
  let rec loop state =
    match S.current_prompt state with
    | None -> S.results state
    | Some prompt -> (
        print_screen io (render_prompt prompt);
        match read_answer io with
        | None -> S.results state
        | Some answer ->
            let feedback, state' = S.submit state answer in
            print_screen io (render_feedback feedback);
            loop state')
  in
  loop session