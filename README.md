# TranTacToe

Tic-Tac-Tran: tic-tac-toe in your terminal, written in Fortran. You play `X`
against the computer's `O`, at one of three difficulty levels. The hardest one
can't be beaten.

```
  ----- ----- -----
 |  O  |  _  |  _  |
  ----- ----- -----
  ----- ----- -----
 |  _  |  X  |  _  |
  ----- ----- -----
  ----- ----- -----
 |  _  |  _  |  _  |
  ----- ----- -----
```

## Download

Prebuilt binaries for Linux (x86_64) and macOS (Apple silicon) are attached to
each [release](https://github.com/ryanromanov/TranTacToe/releases/latest).
Neither needs Fortran installed.

```sh
tar -xzf tictac-macos-arm64.tar.gz   # or tictac-linux-x86_64.tar.gz
./tictac
```

On macOS, a binary downloaded with a browser is blocked as coming from an
unidentified developer. Clear the quarantine flag once to allow it:

```sh
xattr -d com.apple.quarantine tictac
```

## Build from source

You need gfortran 9 or newer (`brew install gcc` on macOS,
`sudo apt install gfortran` on Debian/Ubuntu).

```sh
gfortran TicTac.f90 -o tictac
./tictac
```

`./tictac --version` prints the version.

## How to play

1. Choose a difficulty:
   - **1 (greenhorn):** the computer plays at random.
   - **2 (leftenant):** it completes its own lines and blocks yours.
   - **3 (Navy Seal Copy Pasta):** it plays perfectly and sets traps. The best
     you can do is a tie.
2. Enter each move as `row column`, separated by a space. Rows and columns are
   numbered 1–3 from the top-left corner, so `1 3` is the top-right cell.
3. To give up, enter `0 0` and confirm with `yes`.

## Tests

```sh
python3 tests/test_game.py
```

The tests build the game and play it through stdin, including every possible
game against difficulty 3. CI runs them on every pull request.

## License

[MIT](LICENSE) © 2017–2026 Ryan Romanosky
