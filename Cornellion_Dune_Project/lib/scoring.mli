val score : Pack.answer_entry -> int * string
(** Fixed scoring for the existing demo question bank. *)

val total_depth : Terminal.round_result list -> int
val placeholder_stats : Terminal.stats_view
