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
