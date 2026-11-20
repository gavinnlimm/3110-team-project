# Terminal interface — future work

This document lists work that remains after the initial terminal WIP PR
(Ryan / rls494). The current PR ships screens, scriptable I/O, and a temporary
`Mvp_driver` so the UI is runnable without the other milestone modules.

## Replace the MVP driver

`lib/mvp_driver.ml` is scaffolding. When teammate modules land:

1. Adapt Karan’s game-session module to `Terminal.Session` /
   `Mvp_driver.Game_driver` (or replace that module type with the real API).
2. Load prompts from Gavin’s question pack instead of `builtin_prompts`.
3. Route typed answers through Vinson’s matcher (normalize, nicknames, edit
   distance) instead of exact/alias matching.
4. Use Colin’s rarity scoring for `points` and `rarity_label` on feedback and
   results views.

`App.play_with_driver` is the intended seam: swap the first-class module
argument without rewriting screen renderers.

## Statistics screen

`Terminal.render_stats` is ready. Wire it to saved game history (Vinson) and
aggregate stats (Colin: best depth, average, games played). Remove
`Mvp_driver.placeholder_stats`.

## Question bank menu

Default bank should call the real pack loader. Custom bank should open Gavin’s
custom pack import/parser with clear error messages. The current menu only
prints placeholder messages.

## Timed rounds

Karan’s Lwt-based timers should interrupt `read_answer` (or wrap the play loop)
so a timeout yields a zero-point round and advances. Terminal helpers may need
an async-friendly `io` variant later.

## Integration / polish

- Persist completed games after `render_results`.
- Achievements on the results or stats screen once scoring unlocks them.
- Optional richer TUI (e.g. lambda-term) — not required for MVP.
- End-to-end tests that use real modules with a fixed seed and fixed answers,
  keeping the scripted-`io` approach from `test/test_terminal.ml`.
