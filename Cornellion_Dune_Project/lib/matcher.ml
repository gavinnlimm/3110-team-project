(** Temporary answer normalization and matching moved from Driver. *)

let normalize s =
  let trimmed = String.trim s |> String.lowercase_ascii in
  let buf = Buffer.create (String.length trimmed) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | '0' .. '9' -> Buffer.add_char buf c
      | ' ' | '\t' | '\n' | '\r' ->
          if
            Buffer.length buf > 0
            && Buffer.nth buf (Buffer.length buf - 1) <> ' '
          then Buffer.add_char buf ' '
      | '\'' -> ()
      | _ -> ())
    trimmed;
  Buffer.contents buf |> String.trim

let matches_alias typed alias = normalize typed = normalize alias

let find_answer (prompt : Pack.prompt_entry) typed =
  let rec search (answers : Pack.answer_entry list) =
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
