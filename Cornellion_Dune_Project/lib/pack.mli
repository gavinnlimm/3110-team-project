(** Question packs: the prompts, valid answers, nicknames, and simulated crowd
    counts that a Cornell Krillion game draws from.

    {2 Ownership}
    - Primary: Gavin (gl568) — question bank, pack format, selection
    - Backup: Karan (km2253)

    {2 Status}
    MS2: pack file format, parser with line-numbered errors, the built-in
    default pack ([data/default_pack.txt]), file loading, and seeded selection
    without repeats are implemented and tested ([test/test_pack.ml]).

    {2 Remaining work}
    - Gavin: custom-pack import from the terminal "Question bank" menu (with
      Ryan); personal statistics (with Colin, Vinson).
    - Karan: call [select] from the game session instead of the temporary
      [Driver] rotation.
    - Vinson: match typed input against [answer.canonical] and
      [answer.aliases]. This module only lowercases and trims when it checks
      for clashes; full normalization and typo tolerance belong to the matcher.
    - Colin: compute rarity from [answer.count] and [total_count].

    See [docs/FUTURE_WORK.md] and [docs/PACK_FORMAT.md].

    {2 Invariants}
    Every value of type [t] comes from the parser, so other modules can rely on
    these facts without checking them:
    - A pack has at least one question, and question ids are unique.
    - A question has a non-empty prompt and at least one answer.
    - Every answer count is at least 1, so [total_count q > 0].
    - Within one question, no two answers share a name or nickname, compared
      case-insensitively after trimming. This keeps the matcher from having to
      choose between two answers for the same typed text. *)

(** {1 Types} *)

(** One valid answer to a prompt. *)
type answer = private {
  canonical : string;
      (** Name shown to the player, e.g. ["Okenshields"]. *)
  aliases : string list;
      (** Other accepted spellings and nicknames, e.g. [["Okies"]]. Does not
          repeat [canonical]. *)
  count : int;
      (** Simulated number of players who gave this answer; at least 1. Lower
          counts mean rarer answers. *)
}

(** One prompt and its valid answers. *)
type question = private {
  id : string;
      (** Unique within a pack. Lowercase letters, digits, and hyphens, e.g.
          ["dining-hall"]. *)
  prompt : string;  (** Text shown to the player. *)
  answers : answer list;  (** In file order; never empty. *)
}

type t
(** A validated question pack. *)

(** Why a pack could not be loaded. *)
type error = {
  line : int option;
      (** 1-based line where the problem was found, or [None] for problems
          that are not tied to one line (unreadable file, empty pack). *)
  message : string;  (** Explanation suitable for showing to the player. *)
}

(** {1 Loading} *)

val of_string : string -> (t, error) result
(** [of_string text] parses [text] in the pack format described in
    [docs/PACK_FORMAT.md]. Returns [Error e] for the first problem found;
    [e.line] gives its line number when it has one. Never raises. *)

val load_file : string -> (t, error) result
(** [load_file path] reads and parses the pack at [path]. Returns
    [Error { line = None; _ }] if the file cannot be read, otherwise behaves
    like [of_string] on its contents. Never raises. *)

val default : unit -> t
(** The built-in Cornell pack shipped with the game, compiled in from
    [data/default_pack.txt] so it works from any directory. Raises [Failure]
    only if that file is malformed, which the test suite checks. *)

val error_to_string : error -> string
(** [error_to_string e] is ["Line N: message"], or just [message] when [e.line]
    is [None]. *)

(** {1 Queries} *)

val name : t -> string
(** The pack's [pack:] name, or ["Untitled pack"] if the file has none. *)

val questions : t -> question list
(** All questions in file order. Never empty. *)

val size : t -> int
(** Number of questions. Always at least 1. *)

val find : t -> string -> question option
(** [find p id] is the question in [p] whose id is [id], if any. *)

val total_count : question -> int
(** [total_count q] is the sum of the counts of [q]'s answers. Always at least
    1, so it is safe to divide by. *)

(** {1 Selection} *)

val select : seed:int -> count:int -> t -> question list
(** [select ~seed ~count p] chooses [min count (size p)] distinct questions from
    [p] in a pseudorandom order determined only by [seed]: the same seed and
    pack always give the same list, on any machine or OCaml version. No
    question appears twice. Raises [Invalid_argument] if [count < 0]. *)
