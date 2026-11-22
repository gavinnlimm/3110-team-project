# Cornell Krillion — shared future work

Living checklist for **all** owners and agents. Update this file when you finish
a bullet or discover new integration work. Module headers in `lib/` and `bin/`
point here; keep them in sync when ownership changes.

## Ownership map

| Area | Primary | Backup | Status |
| --- | --- | --- | --- |
| Question bank / packs / selection | Gavin (gl568) | Karan | MS2 done: `Pack` module + default pack + tests; not yet wired into `Driver` |
| Answer matching + game history I/O | Vinson (vc377) | Gavin | Not started (exact/alias stub in `Driver`) |
| Crowd rarity scoring + achievements + stats | Colin (czt4) | Vinson | Not started (fixed points + `placeholder_stats`) |
| Game sessions / timed rounds (Lwt) | Karan (km2253) | Ryan | Not started (seed rotate + untimed loop in `Driver`/`App`) |
| Terminal UI + integration | Ryan (rls494) | Colin | WIP — screens + scriptable I/O shipped |

## Cross-cutting integration seam

`Driver.Game_driver` / `Terminal.Session` is the intended boundary between UI and
game logic. `App.play_with_driver` packs any module matching that shape.

When real modules land:

1. Adapt Karan’s session (or a thin adapter) to `Game_driver` / `Session`.
2. Point `App` at that module instead of the temporary `Driver` implementation.
3. Delete or shrink `lib/driver.ml` so it no longer owns pack/match/score logic.

## By owner

### Gavin (gl568) — question bank

- [x] Define pack file format and ship the default Cornell prompt bucket
      (`docs/PACK_FORMAT.md`, `data/default_pack.txt`, 6 questions).
- [x] Implement pack loader/parser with clear errors for malformed files
      (`lib/pack.mli`: `of_string`, `load_file`, `default`).
- [x] Seeded prompt selection with no repeats within a game (`Pack.select`).
- [ ] Custom question import for the terminal “Question bank” → Custom path
      (with Ryan: `App.handle_bank` calls `Pack.load_file`, shows
      `Pack.error_to_string` on failure).
- [ ] Replace `Driver.builtin_prompts` as the source of truth (with Karan/Ryan:
      session uses `Pack.select`; points come from Colin’s scoring of
      `answer.count`).
- [x] OUnit: valid packs load; malformed packs rejected; selection is seeded
      (`test/test_pack.ml`, 27 tests).
- [ ] Personal statistics with Colin/Vinson once history saving exists.

### Vinson (vc377) — matcher + history

- [ ] Normalize typed input; nickname lookup; safe edit-distance fuzzy match.
- [ ] Guard against confusing close distinct answers (e.g. Becker vs Baker).
- [ ] Save completed games to a history file in an agreed format.
- [ ] Replace exact/alias matching inside today’s temporary `Driver.submit`.
- [ ] OUnit: matching correctness; history round-trip.

### Colin (czt4) — scoring + achievements + stats

- [ ] Rarity tiers and points from simulated crowd frequencies.
- [ ] Total score / depth and per-answer breakdown for results screen.
- [ ] Achievements unlocked from performance.
- [ ] Personal stats (best depth, average, games played) from history.
- [ ] Replace fixed `points`/`rarity_label` in `Driver` and
      `Driver.placeholder_stats`.
- [ ] OUnit: tier boundaries, depth, achievements, stats on fixed samples.

### Karan (km2253) — game sessions

- [ ] Session engine: seed → prompts, submit → match/score, track rounds/depth.
- [ ] End-of-game summary consumed by `Terminal.render_results`.
- [ ] Timed rounds with Lwt; timeout → 0 points and advance.
- [ ] Replace `Driver` session state / `order_prompts` rotation.
- [ ] OUnit: full games with fixed seed and fixed answers; timed behavior.

### Ryan (rls494) — terminal + integration

- [x] Main menu, prompt, feedback, results, stats/bank scaffolds.
- [x] Scriptable `Terminal.io` and OUnit screen + scripted-game tests.
- [x] Temporary `Driver` so the binary runs before other modules exist.
- [ ] Wire real pack / matcher / scoring / session into `App` (drop stub driver).
- [ ] Stats screen backed by real history aggregates (not placeholder).
- [ ] Custom bank flow once Gavin’s importer exists.
- [ ] Timed-round UX once Karan’s Lwt session interrupts answer input.
- [ ] Optional: richer TUI later (not required for MVP).

## Shared polish (anyone / after core modules)

- [ ] Persist every completed game immediately after results.
- [ ] Show achievements on results or stats when Colin unlocks them.
- [ ] End-to-end tests that use **real** modules with fixed seed + answers,
      keeping the scripted-`io` pattern from `test/test_terminal.ml`.
- [ ] Announce `.mli` changes in the team chat before merge (team contract).
