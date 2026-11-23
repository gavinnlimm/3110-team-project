(** Application loop: menus + play-session wiring for Cornell Krillion.

    See [app.mli] and [docs/FUTURE_WORK.md] for ownership and remaining
    integration work. Keep screen code in [Terminal]; keep temporary game logic
    out of this file—swap the packed [Driver] module when real engines land. *)

let play_with_driver (type a) (io : Terminal.io) ~seed
    (module D : Driver.Game_driver with type t = a) =
  let session = D.start ~seed in
  let module S = struct
    type t = a

    let current_prompt = D.current_prompt
    let submit = D.submit
    let results = D.results
  end in
  let results =
    Terminal.run_play_session io (module S : Terminal.Session with type t = a)
      session
  in
  Terminal.print_screen io (Terminal.render_results results);
  results

let handle_bank (io : Terminal.io) =
  match Terminal.read_bank_choice io with
  | Terminal.Default_bank ->
      io.Terminal.print_line "Using the built-in temporary question bank.";
      io.print_line ""
  | Terminal.Custom_bank ->
      io.print_line
        "Custom question-bank import is not implemented yet (Gavin's pack \
         parser).";
      io.print_line ""
  | Terminal.Back -> ()

let handle_stats (io : Terminal.io) =
  Terminal.print_screen io (Terminal.render_stats Scoring.placeholder_stats)

let handle_play (io : Terminal.io) =
  let seed = Terminal.read_seed io in
  ignore (play_with_driver io ~seed (module Session))

let rec run (io : Terminal.io) =
  match Terminal.read_menu_choice io with
  | Terminal.Play ->
      handle_play io;
      run io
  | Terminal.Stats ->
      handle_stats io;
      run io
  | Terminal.Bank ->
      handle_bank io;
      run io
  | Terminal.Quit -> io.print_line "Goodbye!"