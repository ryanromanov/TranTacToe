# BoardLoader (2017 scratch code)

`ticTakScratch.f` was committed alongside the original game on 2017-09-08
(commit `daafce7`). It was an experiment in describing every winning line
with a table instead of hand-written checks. The game never used it: the only
call to it was commented out in the main program. The file was renamed to
`ticTacScratch.f90` during the free-form conversion, then removed from the
build tree and preserved here, unchanged, for historical interest.

## The idea

`vBoard` is a 24×3 table: the 8 winning lines, each stored as a 3×3 mask of
the board, stacked on top of one another. A `1` marks a cell that belongs to
the line:

| Rows of `vBoard` | Line            |
|------------------|-----------------|
| 1–3              | top row         |
| 4–6              | middle row      |
| 7–9              | bottom row      |
| 10–12            | left column     |
| 13–15            | middle column   |
| 16–18            | right column    |
| 19–21            | `\` diagonal    |
| 22–24            | `/` diagonal    |

## How it turned out

Six of the eight masks come out right. The middle and bottom row masks stay
empty: their loops test `r .eq. 2` and `r .eq. 3` while `r` runs over 4–6 and
7–9, so the condition is never true. The table is also never zeroed, so cells
outside the lines hold whatever was in memory.

The same idea now lives in `ScanLine` in `TicTac.f90`, as two `DATA` tables
holding the row and column of each line's three cells. The computer player and
the win check (`VCheck`) are both built on it.

## The code

```fortran
subroutine BoardLoader(r,c,vRows,vCols, vBoard)
  integer :: r,c, vRows,vCols
  integer, dimension(24,3) :: vBoard
  do r = 1, vRows - 3*7
    do c = 1, vCols
      if (r .eq. 1) then
        vBoard(r,c) = 1
      end if
    end do
  end do

  do r = 4, vRows - 3*6
    do c = 1, vCols
      if (r .eq. 2) then
        vBoard(r,c) = 1
      end if
    end do
  end do

  do r = 7, vRows - 3*5
    do c = 1, vCols
      if (r .eq. 3) then
        vBoard(r,c) = 1
      end if
    end do
  end do

  do r = 10, vRows - 3*4
    do c = 1, vCols
      if (c .eq. 1) then
        vBoard(r,c) = 1
      end if
    end do
  end do

  do r = 13, vRows - 3*3
    do c = 1, vCols
      if (c .eq. 2) then
        vBoard(r,c) = 1
      end if
    end do
  end do

  do r = 16, vRows - 3*2
    do c = 1, vCols
      if (c .eq. 3) then
        vBoard(r,c) = 1
      end if
    end do
  end do

  do r = 19, vRows - 3
    do c = 1, vCols
      if (c .eq. (r-18)) then
        vBoard(r,c) = 1
      end if
    end do
  end do

  do r = 22, vRows
    do c = 1, vCols
      if (c .eq. (vRows - r + 1)) then
        vBoard(r,c) = 1
      end if
    end do
  end do

end subroutine BoardLoader
```
