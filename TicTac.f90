! **********************************************************************
!   Program Name:       Tic-Tac-Tran
!   Author:             Ryan Romanosky
!   Version No.:        see VERSION below (released as git tag vX.Y.Z)
!
!
!
!	**********************************************************************
!-----------------------------------------------------------------------
!     Begin Program / Declare Variables
!       Uses:
!         Subroutines:  Menu()
!                       PrintBoard()
!                       GetPos()
!                       MarkBoard()
!                       Sonny()
!                       VCheck()
!                       Turing()
!
!         Variables:    board[3,3]
!                       r,c
!                       rows, cols  (both set to 3)
!                       x,y         (for user input)
!                       victor      (determines victor of match)
!                       counter     (keeps track of turns)
!                       diff        (difficulty setting)
!                       retire      (used to quit program)
!                       VERSION     (release version, see CLAUDE.md)
!-----------------------------------------------------------------------
program tictactran
  implicit none
  ! Release version; must match the git tag (vX.Y.Z) when releasing
  character(len=*), parameter :: VERSION = '0.2.0'
  ! main board array
  character, dimension(3,3) :: board = '_'

  integer :: r,c
  integer :: rows = 3, cols = 3
  ! integer :: vRows = 24, vCols = 3
  integer :: x, y
  integer :: victor = 0
  integer :: counter = 0
  integer :: diff = 0

  ! integer, dimension(24,3) :: vBoard
  character(len=3) :: retire = 'no'
  character(len=16) :: arg
  ! call BoardLoader(r,c,vRows,vCols,vBoard)




  ! "tictac --version" prints the version and exits
  call get_command_argument(1, arg)
  if ( arg .eq. '--version' ) then
    write(*,'(A)') VERSION
    stop
  end if

  ! Seed the random number generator once, differently on each run
  call random_init(.false., .false.)
  call Menu(diff, VERSION)
  do while (retire .ne. 'yes' .and. victor .eq. 0)
    call PrintBoard(board, rows, cols, r, c)

    call GetPos(x,y, counter, retire, board)
    if (retire .ne. 'yes') then
      if (counter .lt. 3) then
        call MarkBoard(x,y,board)
        call Sonny(board, diff)
      else
        call MarkBoard(x,y,board)
        call VCheck(board, rows, cols, victor)
        if ( victor .eq. 0 ) then
          call Sonny(board, diff)
          call VCheck(board, rows, cols, victor)
        end if
      end if
    end if


  end do

  if ( victor .eq. 1 ) then
    write(*,*) "Congratulations! You've beaten the computer!"
    call Turing()
  else if (victor .eq. 2) then
    write(*,*) "Congratulations! You've been beaten by the computer"
    call Turing()
  else if (victor .eq. 3) then
    write(*,*) 'The tie goes to the mortal man.'
    call Turing()
  end if

end program tictactran



! Subroutine for Menu display & gets difficulty level
subroutine Menu(diff, ver)
  integer :: diff
  character(len=*) :: ver
  write(*,*) "Welcome to Tic-Tac-Tran! (v", ver, ")"
  write(*,*) "This Fortran-based emulator is the most accurate and "
  write(*,*) "graphically advanced way to play the game known for its ability " // &
             "to render even the most expensive GPUs into a molten pile of " // &
             "wasted savings accounts."
  write(*,*)

  write(*,*) 'Please choose your difficulty level'
  write(*,*) '(1 being greenhorn, 2 being leftenant, 3 being Navy Seal Copy Pasta)'
  call ReadInt(diff)
    do while (diff .lt. 1 .or. diff .gt. 3)
      write(*,*) 'Error: Please choose a number between 1 and 3'
      call ReadInt(diff)
    end do
  write(*,*) "Fight!"
end subroutine menu

! Subroutine to print tic-tac-toe board
subroutine PrintBoard(board, rows, cols, r, c)
  character, dimension(3,3) :: board
  integer :: r,c
  integer :: rows, cols
  r = 1
  c = 1
  write(*,*)
  do r = 1, rows
    write(*,2)
    write(*,1) (board(r,c), c = 1, cols)
    write(*,2)
  end do
  write(*,*)
1 format(' ', "|",2X,A,2X,"|",2X,A,2X,"|",2X,A,2X,"|")
2 format(' ',  X,5("-"),X,5("-"),X,5("-"))
end subroutine PrintBoard

! Subroutine to get User coördinates
subroutine GetPos(x,y,c, ret,b)
  integer :: x,y, c
  character, dimension(3,3) :: b
  integer :: tempX, tempY
  character(len=3) :: ret
  integer :: ios
  tempX = -1
  tempY = -1
  x = 0
  y = 0

  write(*,*) 'Please enter where you wish to place your marker'
  write(*,*) 'Enter in the format "x y" with a space between them'
  write(*,*) 'Coordinates are 1-indexed and the top-left corner is 1,1'
  write(*,*) '( To retire, enter 0 0 )'
  call ReadPair(tempX,tempY)
  do while ((x .eq. 0 .or. y .eq. 0) .and. ret .ne. 'yes' )
    if ( tempX .gt. 0 .and. tempX .lt. 4 ) then
      if ( tempY .gt. 0 .and. tempY .lt. 4 ) then
        if (b(tempX, tempY) .eq. "_") then
          x = tempX
          y = tempY
          c = c + 1
        else
          call OverLaid(tempX, tempY, b)
          write(*,*) 'Please re-enter your x- and y-coordinates:'
          call ReadPair(tempX,tempY)
        end if
      else
        write(*,*) 'Error: your y-coordinates are wrong'
        write(*,*) 'Please re-enter your x- and y-coordinates:'
        call ReadPair(tempX,tempY)
      end if
    else if (tempX .eq. 0 .and. tempY .eq. 0) then
      write(*,*) 'Are you sure you want to retire? Enter "yes" to retire ' // &
                 'or anything else to continue.'
      read(*,*,iostat=ios) ret
      if ( ios .lt. 0 ) call EndOfInput()
      if ( ret .eq. 'yes' ) then
        call OAP()
      else
        write(*,*) 'Please enter your x- and y-coordinates:'
        call ReadPair(tempX,tempY)
      end if
    else
      write(*,*) 'Error: your x-coordinates are wrong'
      write(*,*) 'Please re-enter your x- and y-coordinates:'
      call ReadPair(tempX,tempY)
    end if
  end do
end subroutine GetPos
! Subroutine to read one integer, re-asking until the input is a number
subroutine ReadInt(n)
  integer :: n, ios
  read(*,*,iostat=ios) n
  do while (ios .ne. 0)
    if ( ios .lt. 0 ) call EndOfInput()
    write(*,*) 'Error: please enter a whole number'
    read(*,*,iostat=ios) n
  end do
end subroutine ReadInt

! Subroutine to read two integers, re-asking until both are numbers
subroutine ReadPair(a, b)
  integer :: a, b, ios
  read(*,*,iostat=ios) a, b
  do while (ios .ne. 0)
    if ( ios .lt. 0 ) call EndOfInput()
    write(*,*) 'Error: please enter two whole numbers, like "2 3"'
    read(*,*,iostat=ios) a, b
  end do
end subroutine ReadPair

! Subroutine to quit cleanly when input ends (Ctrl-D or end of a pipe)
subroutine EndOfInput()
  write(*,*)
  write(*,*) 'No more input. Quitting program...'
  stop
end subroutine EndOfInput

    ! Indented Subroutine to quit
    subroutine OAP()
      write(*,*) 'Quitting program...'
      call Turing()
    end subroutine OAP

    subroutine OverLaid(tX, tY, b)
      integer :: tX, tY
      character, dimension(3,3) :: b
      if ( b(tX,tY) .eq. "X" ) then
        write(*,*) "You've already got an 'X' there mate!"
        write(*,*)
      else
        write(*,*) "You can't go where the AI has gone before..."
        write(*,*)
      end if

    end subroutine OverLaid
! Subroutine to mark board for User
subroutine MarkBoard(x,y,b)
  integer :: x,y
  character, dimension(3,3) :: b

  b(x,y) = "X"
end subroutine MarkBoard


! "The Three Laws will lead to only one logical outcome."
subroutine Sonny(b, diff)
  character, dimension(3,3) :: b
  integer :: y,z
  integer :: diff
  real :: rando
  logical :: found

  found = .false.
  ! "A robot may not harm a human or, by inaction, allow a human being
  ! to come to harm."
  ! Difficulty 1 plays a random empty cell (the fallback below).

  ! "A robot must obey orders given it by human beings except where
  ! such orders would conflict with the first law."
  ! Difficulty 2 completes its own line, else blocks the user's line,
  ! else plays randomly.
  if ( diff .eq. 2 ) then
    call FindLine(b, "O", y, z, found)
    if ( .not. found ) call FindLine(b, "X", y, z, found)
  end if

  ! "A robot must protect its own existence as long as such
  ! protection does not conflict with the first or second law."
  ! Difficulty 3 searches the full game tree and never loses.
  if ( diff .eq. 3 ) then
    call BestMove(b, y, z)
    found = .true.
  end if

  do while (.not. found)
    call random_number(rando)
    y = int(rando*3)+1
    call random_number(rando)
    z = int(rando*3)+1
    found = b(y,z) .eq. "_"
  end do
  b(y,z) = "O"
end subroutine Sonny

! Subroutine to scan winning line i (1-3 rows, 4-6 cols, 7-8 diagonals)
! n:     how many of its cells hold mark
! ey,ez: one of its empty cells, or 0,0 if it has none
subroutine ScanLine(b, i, mark, n, ey, ez)
  character, dimension(3,3) :: b
  character :: mark
  integer :: i, n, ey, ez, k
  integer, dimension(3,8) :: lr, lc
  data lr /1,1,1, 2,2,2, 3,3,3, 1,2,3, 1,2,3, 1,2,3,                         1,2,3, 1,2,3/
  data lc /1,2,3, 1,2,3, 1,2,3, 1,1,1, 2,2,2, 3,3,3,                         1,2,3, 3,2,1/
  n = 0
  ey = 0
  ez = 0
  do k = 1, 3
    if ( b(lr(k,i),lc(k,i)) .eq. mark ) n = n + 1
    if ( b(lr(k,i),lc(k,i)) .eq. "_" ) then
      ey = lr(k,i)
      ez = lc(k,i)
    end if
  end do
end subroutine ScanLine

! Subroutine to find a line with two of mark and one empty cell (y,z)
subroutine FindLine(b, mark, y, z, found)
  character, dimension(3,3) :: b
  character :: mark
  integer :: y, z, i, n, ey, ez
  logical :: found
  found = .false.
  do i = 1, 8
    call ScanLine(b, i, mark, n, ey, ez)
    if ( n .eq. 2 .and. ey .ne. 0 ) then
      y = ey
      z = ez
      found = .true.
      return
    end if
  end do
end subroutine FindLine

! Function to check whether mark holds a complete line
logical function Wins(b, mark)
  character, dimension(3,3) :: b
  character :: mark
  integer :: i, n, ey, ez
  Wins = .false.
  do i = 1, 8
    call ScanLine(b, i, mark, n, ey, ez)
    if ( n .eq. 3 ) Wins = .true.
  end do
end function Wins

! Subroutine to pick the computer's best cell (y,z) by minimax
subroutine BestMove(b, y, z)
  character, dimension(3,3) :: b
  integer :: y, z, r, c, s, best
  logical :: found
  integer :: Score
  ! Take an immediate win rather than any equally-scored slower one
  call FindLine(b, "O", y, z, found)
  if ( found ) return
  best = -2
  do r = 1, 3
    do c = 1, 3
      if ( b(r,c) .eq. "_" ) then
        b(r,c) = "O"
        s = Score(b, "X")
        b(r,c) = "_"
        if ( s .gt. best ) then
          best = s
          y = r
          z = c
        end if
      end if
    end do
  end do
end subroutine BestMove

! Function to score board b with mark to move: 1 O wins, -1 X wins, 0 tie
recursive integer function Score(b, mark) result(best)
  character, dimension(3,3) :: b
  character :: mark, other
  integer :: r, c, s
  logical :: moved
  logical :: Wins
  if ( Wins(b, "O") ) then
    best = 1
    return
  else if ( Wins(b, "X") ) then
    best = -1
    return
  end if

  if ( mark .eq. "O" ) then
    best = -2
    other = "X"
  else
    best = 2
    other = "O"
  end if
  moved = .false.
  do r = 1, 3
    do c = 1, 3
      if ( b(r,c) .eq. "_" ) then
        b(r,c) = mark
        s = Score(b, other)
        b(r,c) = "_"
        moved = .true.
        if ( mark .eq. "O" ) best = max(best, s)
        if ( mark .eq. "X" ) best = min(best, s)
      end if
    end do
  end do
  if ( .not. moved ) best = 0
end function Score

! Subroutine to check for Victory for either player
subroutine VCheck(b, rows, cols, victor)
  character, dimension(3,3) :: b
  integer :: r, c
  integer :: rows, cols
  integer :: victor
  logical :: tie
  integer :: vReturn
  ! check horizontal conditions


  do r = 1, rows
    c = 1
    if (b(r,c) .ne. "_") then
      if ( b(r,c) .eq. b(r,c+1) .and. b(r,c) .eq. b(r,c+2) ) then
        victor = VReturn(b,r,c)
      end if
    end if
  end do
  ! check vertical conditions
  do c = 1, cols
    r = 1
    if (b(r,c) .ne. "_") then
      if (b(r,c) .eq. b(r+1,c) .and. b(r,c) .eq. b(r+2,c)) then
        victor = VReturn(b,r,c)
      end if
    end if
  end do
  ! czech diagonals
  ! check downward diagonal
  r = 1
  c = 1

  if (b(r,c) .ne. "_") then
    if (b(r,c) .eq. b(r+1,c+1) .and. b(r,c) .eq. b(r+2,c+2)) then
      victor = VReturn(b,r,c)
    end if
  end if
  ! check upward diagonal
  if (b(r+2,c) .ne. "_") then
    if (b(r+2,c) .eq. b(r+1,c+1) .and. b(r,c+2) .eq. b(r+2,c))then
      victor = VReturn(b, r+2, c)
    end if
  end if


  if ( victor .eq. 0 ) then
    tie = .true.
    do r = 1, rows
      do c = 1, cols
        if ( b(r,c) .eq. "_" ) then

          tie = .false.
        end if
      end do
    end do
    if ( tie .eqv. .true. ) then

      victor = 3
    end if
  end if
end subroutine VCheck

! Function to return who the victor is
integer function VReturn(b,r,c)
  character, dimension(3,3) :: b
  integer :: r, c

  if (b(r,c) .eq. "X") then
    vReturn = 1
  else if (b(r,c) .eq. "O") then
    vReturn = 2
  end if
end function vReturn


subroutine Turing()
  integer :: coin
  real :: temp


  call random_number(temp)
  coin = int(temp*10)+1
  if (coin .lt. 3) then
    write(*,*)
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)
    write(*,*) "  Machines take me by surprise with great frequency."
    write(*,*) "                                      - Alan Turing"
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)
  else if (coin .lt. 5) then
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)
    write(*,*) "  We are not interested in the fact that the brain has"
    write(*,*) "the consistency of cold porridge."
    write(*,*) "                                         - Alan Turing"
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)
  else if (coin .lt. 7) then
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)
    write(*,*) "I propose to consider the question,"
    write(*,*) "          'Can machines think?'"
    write(*,*) "                     - Alan Turing"
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)

  else if (coin .lt. 9) then
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)
    write(*,*) "Det. Spooner: Human beings have dreams. Even dogs have dreams, but not"
    write(*,*) "              you, you are just a machine. An imi tation of life."
    write(*,*) "Can a robot write a symphony? Can a robot turn a... " // &
               "canvas into a beautiful masterpiece?"
    write(*,*) "Sonny: Can *you*?"
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)
  else
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*) "Software-industry battles are fought by highly paid and " // &
               "out-of-shape nerds furiously pounding computer keyboards " // &
               "while they guzzle diet Coke."
    write(*,*) "                                - Nathan Myhrvold"
    write(*,*)
    write(*,*) "*******************************************************"
    write(*,*)
  end if
end subroutine Turing
