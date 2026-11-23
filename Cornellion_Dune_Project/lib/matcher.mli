val normalize : string -> string
(** Existing demo answer matcher. *)

val matches_alias : string -> string -> bool
val find_answer : Pack.prompt_entry -> string -> Pack.answer_entry option
