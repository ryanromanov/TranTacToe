# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Tic-Tac-Tran: a terminal tic-tac-toe game written in Fortran. The human plays `X`, the computer plays `O`. There is no build system or linter; CI (`.github/workflows/ci.yml`) runs the end-to-end tests on every PR and push to `master`.

## Build & run

```sh
gfortran TicTac.f -o tictac   # fixed-form source (.f); needs gfortran 9+ for random_init
./tictac
```

```sh
python3 tests/test_game.py                                  # build with -Wall -fcheck=all, run all tests
python3 tests/test_game.py Input.test_declining_retire_resumes_game   # one test
```

The tests drive the game over stdin (difficulty, then moves as `row col`; `0 0` + `yes` retires) and classify the outcome from its output. They also fail on any code line past column 72. Random difficulties are run 20 times per test; difficulty 3 is checked against every possible user move sequence.

## Source layout

- `TicTac.f` — the whole game. **Fixed-form Fortran**: code starts at column 7, `c` in column 1 is a comment, continuation lines have `+` in column 6, and anything past column 72 is ignored. Keep long string literals split with `+` continuations as the existing code does.
- `ticTakScratch.f` — scratch/experimental `BoardLoader` subroutine (a 24×3 win-pattern table) written in **free-form** despite the `.f` extension; it only compiles with `gfortran -ffree-form`. It is not linked into the game; the call to it in `TicTac.f` is commented out.

## Architecture (TicTac.f)

The main program loops until the user retires or `victor` is non-zero:
`Menu` (pick difficulty 1–3) → loop { `PrintBoard` → `GetPos` (read/validate user move, increments turn `counter`) → `MarkBoard` → `VCheck` (only once `counter >= 3`) → `Sonny` (computer move) → `VCheck` } → end message + `Turing` (random quote).

- Board is `character board(3,3)`, empty cells are `'_'`. User input `x y` maps to `board(x,y)` i.e. **x = row, y = column**.
- `victor` codes: `0` none, `1` user (X), `2` computer (O), `3` tie. `VCheck` sets it via the `VReturn` function.
- `Sonny` is the AI, with one block per difficulty (comments quote Asimov's Three Laws):
  - `diff == 1`: random empty cell.
  - `diff == 2`: completes its own line, else blocks the user's (`FindLine`), else random.
  - `diff == 3`: full minimax search (`BestMove` → recursive `Score`); deterministic and unbeatable.
  - The 8 winning lines live in one table in `ScanLine`; `FindLine` and `Wins` are built on it. `VCheck` still has its own separate line checks.
  - `Sonny` assumes at least one empty cell; the main loop only calls it while `victor == 0`.
- Subroutines take array/dimension arguments explicitly (no modules); names are referenced implicitly, so signature changes must be updated at every call site manually.
- Only the main program has `implicit none`. Inside subroutines, a function such as `Wins` or `Score` must be declared with its type in every caller (`logical :: Wins`), or it is implicitly typed `real`.
