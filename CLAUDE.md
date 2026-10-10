# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Tic-Tac-Tran: a terminal tic-tac-toe game written in Fortran. The human plays `X`, the computer plays `O`. There is no build system or linter; CI (`.github/workflows/ci.yml`) runs the end-to-end tests on every PR and push to `master`.

## Build & run

```sh
gfortran TicTac.f90 -o tictac   # needs gfortran 9+ for random_init; also writes game.mod (gitignored)
./tictac
```

```sh
python3 tests/test_game.py                                  # build (warnings are errors), run all tests
python3 tests/test_game.py Input.test_declining_retire_resumes_game   # one test
```

The tests drive the game over stdin (difficulty, then moves as `row col`; `0 0` + `yes` retires) and classify the outcome from its output. They also fail on any line over 132 characters (the standard free-form limit). Random difficulties are run 20 times per test; difficulty 3 is checked against every possible user move sequence.

## Versioning & releases

The version lives in one place: `VERSION` in the main program of `TicTac.f90` (semver, e.g. `0.2.0`). `./tictac --version` prints it and the welcome banner shows it.

To release: bump `VERSION` in a PR and merge it, then tag that commit on `master`: `git tag v0.3.0 && git push origin v0.3.0`. `.github/workflows/release.yml` re-runs the tests, fails if the tag doesn't equal `v` + `VERSION`, builds Linux x86_64 (fully static) and macOS arm64 (static libgfortran/libgcc/libquadmath) binaries, and publishes a GitHub Release with notes generated from merged PRs. PRs that edit `release.yml` run the build jobs without publishing.

## Source layout

- `TicTac.f90` — the whole game, free-form Fortran (`!` comments, `&` at line end to continue): `module game` with every subroutine and function, then the main program, which must come after the module it `use`s. Split long messages with `"..." // &` so lines stay within 132 characters.
- `docs/history/board-loader.md` — the original 2017 scratch `BoardLoader` code, kept for history. It is not part of the build.

## Architecture (TicTac.f90)

The main program loops until the user retires or `victor` is non-zero:
`Menu` (pick difficulty 1–3) → loop { `PrintBoard` → `GetPos` (read/validate user move, increments turn `counter`) → `MarkBoard` → `VCheck` (only once `counter >= 3`) → `Sonny` (computer move) → `VCheck` } → end message + `Turing` (random quote).

- Board is `character board(3,3)`, empty cells are `'_'`. User input `x y` maps to `board(x,y)` i.e. **x = row, y = column**.
- Read numbers through `ReadInt` / `ReadPair`, never a bare `read(*,*)`: they re-prompt on non-numeric input and call `EndOfInput` (clean quit) when stdin ends.
- `victor` codes: `0` none, `1` user (X), `2` computer (O), `3` tie. `VCheck` sets it from `Wins`.
- `Sonny` is the AI, with one block per difficulty (comments quote Asimov's Three Laws):
  - `diff == 1`: random empty cell.
  - `diff == 2`: completes its own line, else blocks the user's (`FindLine`), else random.
  - `diff == 3`: full minimax search (`BestMove` → recursive `Score`); deterministic and unbeatable.
  - The 8 winning lines live in one table in `ScanLine`; `FindLine`, `Wins` and `VCheck` are built on it.
  - `Sonny` assumes at least one empty cell; the main loop only calls it while `victor == 0`.
- Subroutines take the board as an explicit `character b(3,3)` argument, and every dummy argument has an `intent`.
- The module and the program both use `implicit none`, so every variable must be declared. Module procedures already have explicit interfaces: don't redeclare a function's type in a caller (`logical :: Wins` is now an error).
