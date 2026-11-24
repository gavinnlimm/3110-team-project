# 3110-team-project
CS 3110 Fall 2026 team project (OCaml)

## Team members
- Gavin Lim (gl568)
- Vinson Chen (vc377)
- Karan Singh Madia (km2253)
- Ryan Siglag (rls494)
- Colin Tremblay (czt4)

## Build / test / run

Requires OCaml and Dune (OUnit2 for tests). From the project root:

```bash
cd Cornellion_Dune_Project
dune build          # compile
dune runtest        # run OUnit suite
dune exec ./bin/main.exe   # play Cornell Krillion (must be .exe, not .ml)
```

Or: `dune exec Cornellion_Dune_Project`
