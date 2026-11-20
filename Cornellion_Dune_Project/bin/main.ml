(** Cornell Krillion executable entry point.

    {2 Ownership}
    - Primary: Ryan (rls494) — terminal + integration
    - Backup: Colin (czt4)

    {2 Status}
    Starts the interactive [App] loop on real stdin/stdout.

    {2 Remaining work (by owner)}
    - Ryan: keep this thin; configuration flags (seed, pack path, history path)
      can be added here later without putting game logic in [main].
    - Gavin: CLI / argv for custom pack path once the importer exists.
    - Vinson: default history file location if saving is configured from main.
    - Karan / Colin: no direct changes expected unless the process needs Lwt
      main-loop setup for timed rounds.

    See [docs/FUTURE_WORK.md]. *)

open Cornellion_Dune_Project

let () = App.run Terminal.stdio
