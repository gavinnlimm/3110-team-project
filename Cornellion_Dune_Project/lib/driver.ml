(** Game session state and round progression for Cornell Krillion.

    Temporary question data, matching, and scoring are delegated to [Pack],
    [Matcher], and [Scoring] without changing demo behavior. *)

module type Game_driver = sig
  type t

  val start : seed:int -> t
  val current_prompt : t -> Terminal.prompt_view option
  val submit : t -> string -> Terminal.feedback_view * t
  val results : t -> Terminal.game_results
end

type t = {
  seed : int;
  prompts : Pack.prompt_entry list;
  index : int;  (** next prompt index to show *)
  rounds : Terminal.round_result list;  (** completed, oldest first *)
}

let start ~seed =
  { seed; prompts = Pack.order_prompts seed; index = 0; rounds = [] }

let current_prompt (state : t) =
  let total = List.length state.prompts in
  if state.index >= total then None
  else
    let prompt = List.nth state.prompts state.index in
    Some { Terminal.index = state.index + 1; total; text = prompt.text }

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
      match Matcher.find_answer prompt typed with
      | Some a ->
          ( {
              Terminal.accepted = true;
              typed;
              understood_as = Some a.canonical;
              points = fst (Scoring.score a);
              rarity_label = Some (snd (Scoring.score a));
            },
            {
              Terminal.prompt_text = prompt.text;
              typed;
              understood_as = Some a.canonical;
              points = fst (Scoring.score a);
              rarity_label = Some (snd (Scoring.score a));
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
      { state with index = state.index + 1; rounds = state.rounds @ [ round ] }
    in
    (feedback, state')

let results (state : t) =
  let total_depth = Scoring.total_depth state.rounds in
  { Terminal.seed = state.seed; rounds = state.rounds; total_depth }
