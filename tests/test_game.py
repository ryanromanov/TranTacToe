"""End-to-end tests for TicTac.f: build it, then drive the game over stdin.

Run with: python3 tests/test_game.py
"""
import os
import shutil
import subprocess
import tempfile
import unittest
from concurrent.futures import ThreadPoolExecutor

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SOURCE = os.path.join(ROOT, "TicTac.f")
ALL_CELLS = [(r, c) for r in range(1, 4) for c in range(1, 4)]
RETIRE = [(0, 0)]
TIMEOUT = 5
RANDOM_RUNS = 20  # random difficulties are re-run to catch intermittent hangs


def setUpModule():
    global BIN, _tmp
    _tmp = tempfile.mkdtemp()
    BIN = os.path.join(_tmp, "tictac")
    subprocess.run(
        ["gfortran", "-Wall", "-Wextra", "-Werror", "-fcheck=all",
         SOURCE, "-o", BIN], check=True
    )


def tearDownModule():
    shutil.rmtree(_tmp)


def play(diff, moves, extra=""):
    """Run one game; returns (outcome, stdout). Moves are (row, col) pairs."""
    inp = f"{diff}\n" + "".join(f"{r} {c}\n" for r, c in moves) + extra
    try:
        p = subprocess.run(
            [BIN], input=inp, capture_output=True, text=True, timeout=TIMEOUT
        )
    except subprocess.TimeoutExpired:
        return "hang", ""
    out = p.stdout + p.stderr
    for outcome, marker in (
        ("runtime_error", "Fortran runtime error"),
        ("user_win", "beaten the computer"),
        ("computer_win", "been beaten by"),
        ("tie", "mortal man"),
        ("quit", "Quitting program"),
    ):
        if marker in out:
            return outcome, out
    return f"other(rc={p.returncode})", out


def last_board(out):
    rows = [l for l in out.splitlines() if l.strip().startswith("|")][-3:]
    return [[cell.strip() for cell in row.split("|")[1:4]] for row in rows]


class SourceFormat(unittest.TestCase):
    def test_no_code_past_column_72(self):
        # Fixed-form Fortran silently drops anything past column 72.
        with open(SOURCE) as f:
            long_lines = [
                i for i, line in enumerate(f, 1)
                if len(line.rstrip("\n")) > 72 and line[:1] not in "cC!"
            ]
        self.assertEqual(long_lines, [])


class Version(unittest.TestCase):
    def test_version_flag_prints_semver(self):
        p = subprocess.run(
            [BIN, "--version"], capture_output=True, text=True, timeout=TIMEOUT
        )
        self.assertEqual(p.returncode, 0)
        self.assertRegex(p.stdout, r"^\d+\.\d+\.\d+\n$")

    def test_banner_shows_version(self):
        version = subprocess.run(
            [BIN, "--version"], capture_output=True, text=True, timeout=TIMEOUT
        ).stdout.strip()
        _, out = play(1, RETIRE, "yes\n")
        self.assertIn(f"(v{version})", out)


class Input(unittest.TestCase):
    def test_board_prints_and_retire_quits(self):
        outcome, out = play(1, RETIRE, "yes\n")
        self.assertEqual(outcome, "quit")
        self.assertIn("|  _  |  _  |  _  |", out)

    def test_declining_retire_resumes_game(self):
        outcome, out = play(1, RETIRE, "no\n1 1\n0 0\nyes\n")
        self.assertEqual(outcome, "quit")
        self.assertEqual(last_board(out)[0][0], "X")

    def test_letters_at_difficulty_prompt(self):
        p = subprocess.run(
            [BIN], input="abc\n1\n0 0\nyes\n", capture_output=True,
            text=True, timeout=TIMEOUT,
        )
        self.assertIn("please enter a whole number", p.stdout)
        self.assertIn("Quitting program", p.stdout)
        self.assertNotIn("Fortran runtime error", p.stdout + p.stderr)

    def test_letters_at_move_prompt(self):
        outcome, out = play(1, [], "a b\n1 x\n2 2\n0 0\nyes\n")
        self.assertEqual(outcome, "quit")
        self.assertEqual(out.count("please enter two whole numbers"), 2)
        self.assertEqual(last_board(out)[1][1], "X")

    def test_end_of_input_quits_cleanly(self):
        for extra in ("", "0 0\n"):  # at the move prompt, at the retire prompt
            with self.subTest(extra=extra):
                outcome, out = play(1, [], extra)
                self.assertEqual(outcome, "quit")
                self.assertIn("No more input", out)

    def test_out_of_range_after_occupied_cell(self):
        outcome, out = play(1, [(1, 1), (1, 1), (5, 5)] + RETIRE, "yes\n")
        self.assertEqual(outcome, "quit")
        self.assertIn("x-coordinates are wrong", out)


class Games(unittest.TestCase):
    def assert_finishes(self, diff, moves, extra="", allowed=None):
        allowed = allowed or {"user_win", "computer_win", "tie", "quit"}
        with ThreadPoolExecutor() as pool:
            results = list(pool.map(
                lambda _: play(diff, moves, extra)[0], range(RANDOM_RUNS)
            ))
        self.assertTrue(set(results) <= allowed, results)

    def test_sweep_every_cell(self):
        # User tries every cell in order; occupied ones are re-prompted.
        for diff in (1, 2, 3):
            with self.subTest(diff=diff):
                self.assert_finishes(diff, ALL_CELLS * 3)

    def test_difficulty_2_corner_opening(self):
        self.assert_finishes(2, [(1, 3)] + RETIRE, "yes\n", {"quit"})

    def test_difficulty_3_never_loses(self):
        # Difficulty 3 is deterministic, so explore every user move sequence.
        tally = {}
        frontier = [[]]
        with ThreadPoolExecutor() as pool:
            while frontier:
                results = pool.map(
                    lambda m: (m, play(3, m + RETIRE, "yes\n")), frontier
                )
                frontier = []
                for moves, (outcome, out) in results:
                    if outcome == "quit":
                        board = last_board(out)
                        frontier += [
                            moves + [(r, c)] for r, c in ALL_CELLS
                            if board[r - 1][c - 1] == "_"
                        ]
                    else:
                        tally[outcome] = tally.get(outcome, 0) + 1
                        self.assertNotEqual(outcome, "user_win", moves)
                        self.assertIn(outcome, ("computer_win", "tie"), moves)
        self.assertTrue(tally)  # the tree was actually explored


if __name__ == "__main__":
    unittest.main(verbosity=2)
