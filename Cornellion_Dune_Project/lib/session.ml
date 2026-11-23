(** Game sessions. See [session.mli] for the rules a game follows.

    A session is never changed in place: [submit] returns a new state. The
    questions still to ask are kept as a list whose head is the current
    question, and finished rounds are kept newest first so adding one is
    constant time; [results] puts them back in order. *)

type t = {
  seed : int;
  total : int;  (** number of questions in this game *)
  remaining : Pack.question list;  (** head is the current question *)
  played : Terminal.round_result list;  (** finished rounds, newest first *)
}

let rounds_per_game = 5

let start_with ~seed ~rounds pack =
  if rounds < 0 then invalid_arg "Session.start_with: rounds must be >= 0";
  let questions = Pack.select ~seed ~count:rounds pack in
  { seed; total = List.length questions; remaining = questions; played = [] }

let start ~seed = start_with ~seed ~rounds:rounds_per_game (Pack.default ())
let is_over s = s.remaining = []

let current_prompt s =
  match s.remaining with
  | [] -> None
  | q :: _ ->
      Some
        {
          Terminal.index = List.length s.played + 1;
          total = s.total;
          text = q.Pack.prompt;
        }

(** {1 Matching and scoring} *)

let match_answer (q : Pack.question) typed =
  let names (a : Pack.answer) = a.canonical :: a.aliases in
  List.find_opt
    (fun a -> List.exists (Matcher.matches_alias typed) (names a))
    q.answers

(** [counts q] is [q]'s answers in the form [Scoring] rates. Packs have no
    hand-picked gem yet, so [gem] is always [false]. *)
let counts (q : Pack.question) =
  List.map
    (fun (a : Pack.answer) ->
      { Scoring.canonical = a.canonical; count = a.count; gem = false })
    q.answers

let score q (a : Pack.answer) =
  match Scoring.score_answer (Scoring.rate_prompt (counts q)) a.canonical with
  | Some s -> s
  | None -> invalid_arg "Session.score: answer is not from this question"

(** {1 Rounds} *)

(** [play_round q typed] is the feedback and the recorded round for answering
    [q] with [typed]. *)
let play_round (q : Pack.question) typed =
  let understood_as, points, rarity_label =
    match match_answer q typed with
    | None -> (None, 0, None)
    | Some a ->
        let s = score q a in
        (Some a.canonical, s.points, Some (Scoring.string_of_tier s.tier))
  in
  let feedback =
    {
      Terminal.accepted = understood_as <> None;
      typed;
      understood_as;
      points;
      rarity_label;
    }
  in
  let round =
    { Terminal.prompt_text = q.prompt; typed; understood_as; points; rarity_label }
  in
  (feedback, round)

let submit s typed =
  match s.remaining with
  | [] ->
      let feedback =
        {
          Terminal.accepted = false;
          typed;
          understood_as = None;
          points = 0;
          rarity_label = None;
        }
      in
      (feedback, s)
  | q :: rest ->
      let feedback, round = play_round q typed in
      (feedback, { s with remaining = rest; played = round :: s.played })

let results s =
  let rounds = List.rev s.played in
  { Terminal.seed = s.seed; rounds; total_depth = Scoring.depth rounds }
