! **********************************************************************
!   Program Name:       Tic-Tac-Tran
!   Author:             Ryan Romanosky
!   Version No.:        0.1
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
!                       values      (used for random_seed)
!                       seed        (used to store random seed)
!                       retire      (used to quit program)
!-----------------------------------------------------------------------
      program tictactran
        implicit none
c   main board array
        character, dimension(3,3) :: board = '_'

        integer :: r,c
        integer :: rows = 3, cols = 3
c        integer :: vRows = 24, vCols = 3
        integer :: x, y
        integer :: victor = 0
        integer :: counter = 0
        integer :: diff = 0

        integer, dimension(8) :: values
        integer :: seed

c        integer, dimension(24,3) :: vBoard
        character(len=3) :: retire = 'no'
c        call BoardLoader(r,c,vRows,vCols,vBoard)




        call Menu(diff)
        do while (retire .ne. 'yes' .and. victor .eq. 0)
          call PrintBoard(board, rows, cols, r, c)

          call GetPos(x,y, counter, retire, board)
          if (retire .ne. 'yes') then
            if (counter .lt. 3) then
              call MarkBoard(x,y,board)
              call Sonny(board, rows, cols,diff)
            else
              call MarkBoard(x,y,board)
              call VCheck(board, rows, cols, victor)
              print*, 'Victor: ', victor
              if ( victor .ne. 3 ) then
                call Sonny(board, rows, cols,diff)
                call VCheck(board, rows, cols, victor)
              end if
            end if
          end if


        end do

        if ( victor .eq. 1 ) then
          write(*,*) "Congratulations! You've beaten the computer!"
          call Turing()
        else if (victor .eq. 2) then
          write(*,*) "Congratulations! You've been beaten by the compute
     +r"
          call Turing()
        else if (victor .eq. 3) then
          write(*,*) 'The tie goes to the mortal man.'
          call Turing()
        end if

      end program tictactran



c Subroutine for Menu display & gets difficulty level
      subroutine Menu(diff)
        integer :: diff
        write(*,*) "Welcome to Tic-Tak-Tran!"
        write(*,*) "This Fortran-based emulator is the most accurate and
     + "
        write(*,*) "graphically advanced way to play the game known for
     +its ability to render even the most expensive GPUs into a molten p
     +ile of wasted savings accounts."
        write(*,*)

        write(*,*) 'Please choose your difficulty level'
        write(*,*) '(1 being greenhorn, 2 being leftenant, 3 being Navy
     +Seal Copy Pasta)'
        read(*,*) diff
          do while (diff .lt. 1 .or. diff .gt. 3)
            write(*,*) 'Error: Please choose a number between 1 and 3'
            read(*,*) diff
          end do
        write(*,*) "Fight!"
      end subroutine menu

c Subroutine to print tic-tac-toe board
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
  1   format(' ', "|",XX,A,XX,"|",XX,A,XX,"|",XX,A,XX,"|")
  2   format(' ',  X,5("-"),X,5("-"),X,5("-"))
      end subroutine PrintBoard

c Subroutine to get User coördinates
      subroutine GetPos(x,y,c, ret,b)
        integer :: x,y, c
        character, dimension(3,3) :: b
        integer :: tempX, tempY
        character(len=3) :: ret
        tempX = -1
        tempY = -1
        x = 0
        y = 0

        write(*,*) 'Please enter where you wish to place your marker'
        write(*,*) 'Enter in the format "x y" with a space between them'
        write(*,*) 'Coordinates are 1-indexed and the top-left corner is
     + 1,1'
        write(*,*) '( To retire, enter 0 0 )'
        read(*,*) tempX,tempY
        do while ((x .eq. 0 .or. y .eq. 0) .and. ret .ne. 'yes' )
c          print*, '//', tempX, tempY
          if ( tempX .gt. 0 .and. tempX .lt. 4 ) then
            if ( tempY .gt. 0 .and. tempY .lt. 4 ) then
              if (b(tempX, tempY) .eq. "_") then
                x = tempX
                y = tempY
                c = c + 1
              else
                do while (b(tempX,tempY) .ne. "_")
                call OverLaid(tempX, tempY, b)
                write(*,*) 'Please re-enter your x- and y-coordinates:'
                read(*,*) tempX,tempY
                end do
              end if
            else
              write(*,*) 'Error: your y-coordinates are wrong'
              write(*,*) 'Please re-enter your x- and y-coordinates:'
              read(*,*) tempX,tempY
            end if
          else if (tempX .eq. 0 .and. tempY .eq. 0) then
            write(*,*) 'Are you sure you want to retire? Enter "yes" to
     +retire or anything else to continue.'
            read(*,*) ret
            if ( ret .eq. 'yes' ) then
              call OAP()
            end if
          else
            write(*,*) 'Error: your x-coordinates are wrong'
            write(*,*) 'Please re-enter your x- and y-coordinates:'
            read(*,*) tempX,tempY
          end if
        end do
        write(*,3) 'x,y =', x,y
        write(*,4) 'counter =', c
  3   format(' ', A,X,I1,',',I1)
  4   format(' ', A,X,I2)
      end subroutine GetPos
c Indented Subroutine to quit
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
c Subroutine to mark board for User
      subroutine MarkBoard(x,y,b)
        integer :: x,y
        character, dimension(3,3) :: b

        b(x,y) = "X"
      end subroutine MarkBoard


c   "The Three Laws will lead to only one logical outcome."
      subroutine Sonny(b, rows, cols, diff)
        character, dimension(3,3) :: b
        integer :: y,z, r, c
        integer :: check
        integer :: rows, cols
        integer :: diff
        real :: rando
        integer :: temp
        integer :: i_seed
        integer, dimension(:), allocatable :: a_seed
        integer, dimension(1:8) :: dt_seed
c Stack used to randomly seed the pseudorandom sequence from rand num gen
        call random_seed(size=i_seed)
        allocate(a_seed(1:i_seed))
        call random_seed(get=a_seed)
        call date_and_time(values=dt_seed)
        a_seed(i_seed)=dt_seed(8)
        a_seed(1)=dt_seed(8)*dt_seed(7)*dt_seed(6)
        call random_seed(put=a_seed)
        deallocate(a_seed)


        check = 0
c   "A robot may not harm a human or, by inaction, allow a human being
c     to come to harm."
        if ( diff .eq. 1 ) then
          do while (check .eq. 0)

            call random_number(rando)
            print*, rando
            y = int(rando*3)+1
            call random_number(rando)
            print*, rando
            z = int(rando*3)+1
            if ( b(y,z) .eq. "_" ) then
              b(y,z) = "O"
              check = 1
            end if
          end do
        end if

c   "A robot must obey orders given it by human beings except where
c     such orders would conflict with the first law."
        if ( diff .eq. 2 ) then
          do while (check .eq. 0)
            call random_number(rando)
!            temp = int(rando*100)+1
!            print*, 'Percentage Chance: ', temp
!
!            if ( temp .eq. 1) then
!              do while (check .eq. 0)
!                call random_number(rando)
!                print*, rando
!                y = int(rando*3)+1
!
!                call random_number(rando)
!                print*, rando
!                z = int(rando*3)+1
!                if ( b(y,z) .eq. "_" ) then
!                  b(y,z) = "O"
c                  check = 1
c                end if
c              end do

c            else
              r = 2
              c = 2
c   AI if user has chosen center
              if (b(r,c) .eq. "X") then
                do while (check .eq. 0)
                  if (b(r, c+1) .eq. "X" .and. b(r,c-1) .eq. "_") then
                      y = r
                      z = c-1
                      b(y,z) = "O"
                      check = 1
                  else if(b(r-1,c) .eq. "X" .and. b(r+1,c) .eq. "_")then
                      y = r+1
                      z = c
                      b(y,z) = "O"
                      check = 1
                  else if(b(r,c-1) .eq. "X" .and. b(r,c+1) .eq. "_")then
                      y = r
                      z = c+1
                      b(y,z) = "O"
                      check = 1
                  else if(b(r+1,c) .eq. "X" .and. b(r-1,c) .eq. "_")then
                      y = r-1
                      z = c
                      b(y,z) = "O"
                      check = 1
                  else if(b(r-1,c-1).eq."X" .and. b(r+1,c+1).eq."_")then
                      y = r+1
                      z = c+1
                      b(y,z) = "O"
                      check = 1
                  else if(b(r-1,c+1).eq."X" .and. b(r+1,c-1).eq."_")then
                      y = r+1
                      z = c-1
                      b(y,z) = "O"
                      check = 1
                  else if(b(r+1,c+1).eq."X" .and. b(r-1,c-1).eq."_")then
                      y = r-1
                      z = c-1
                      b(y,z) = "O"
                      check = 1
                  else if(b(r+1,c-1).eq."X" .and. b(r-1,c+1).eq."_")then
                      y = r-1
                      z = c+1
                      b(y,z) = "O"
                      check = 1
                  else
                    do while (check .eq. 0)
                      print*, 'czech for center-chosen non aligned'
                      call random_number(rando)
                      y = int(rando*3)+1
                      call random_number(rando)
                      z = int(rando*3)+1
                      print*, 'Board marked: ', y,z
                      if ( b(y,z) .eq. "_" ) then
                        b(y,z) = "O"
                        check = 1
                      end if
                    end do

                  end if

                end do
c AI if user hasn't chosen the center
              else
                do while ( check .eq. 0 )
                  if ( b(1,1) .eq. "X" .and. b(1,2) .eq. "X" .and.
     +b(1,3) .eq. "_" ) then
                    b(1,3) = "O"
                    check = 1

                  else if (b(1,1) .eq. "X" .and. b(2,1) .eq. "X" .and.
     +b(3,1) .eq. "_") then
                    b(3,1) = "O"
                    check = 1

                  else if (b(1,1) .eq. "X" .and. b(3,1) .eq. "X" .and.
     +b(2,1) .eq. "_") then
                    b(2,1) = "O"
                    check = 1

                  else if (b(1,1) .eq. "X" .and. b(1,3) .eq. "X" .and.
     +b(1,2) .eq. "_") then
                    b(1,2) = "O"
                    check = 1

                  else if (b(3,3) .eq. "X" .and. b(3,2) .eq. "X" .and.
     +b(3,1) .eq. "_") then
                    b(3,1) = "O"
                    check = 1
                  else if (b(3,3) .eq. "X" .and. b(3,1) .eq. "X" .and.
     +b(3,2) .eq. "_") then
                    b(3,2) = "O"
                    check = 1
                  else if (b(3,3) .eq. "X" .and. b(2,3) .eq. "X" .and.
     +b(1,3) .eq. "_") then
                    b(1,3) = "O"
                    check = 1
                  else if (b(3,3) .eq. "X" .and. b(1,3) .eq. "X" .and.
     +b(2,3) .eq. "_") then
                    b(2,3) = "O"
                    check = 1

                  else if (b(1,3) .eq. "X" ) then
                    if ( b(1,2) .eq. "X" .and. b(1,1) .eq. "_" ) then
                      b(1,1) = "O"
                      check = 1

                    else if (b(2,3) .eq. "X" .and. b(3,3) .eq. "_") then
                      b(3,3) = "O"
                      check = 1
                    end if
                  else if (b(3,1) .eq. "X") then
                    if ( b(3,2) .eq. "X" .and. b(3,3) .eq. "_" ) then
                      b(3,3) = "O"
                      check = 1
                    else if (b(2,1) .eq. "X" .and. b(1,1) .eq. "_") then
                      b(1,1) = "O"
                      check = 1
                    end if

                  else if (b(1,1) .eq. "X" .and. b(3,3) .eq. "X" .and.
     +b(2,2) .eq. "_") then
                    b(2,2) = "O"
                    check = 1
                  else if (b(1,1) .eq. "X" .and. b(1,2) .eq. "X" .and.
     +b(3,3) .eq. "_") then
                    b(3,3) = "O"
                    check = 1
                  else if (b(1,3) .eq. "X" .and. b(3,1) .eq. "X" .and.
     +b(2,2) .eq. "_") then
                    b(2,2) = "O"
                  else if (b(1,3) .eq. "X" .and. b(2,2) .eq. "X" .and.
     +b(3,1) .eq. "_") then
                    b(3,1) = "O"
                    check = 1
                  else if (b(3,3) .eq. "X" .and. b(2,2) .eq. "X" .and.
     +b(1,1) .eq. "_") then
                    b(1,1) = "O"
                    check = 1
                  else if (b(3,1) .eq. "X" .and. b(2,2) .eq. "X" .and.
     +b(1,3) .eq. "_") then
                    b(1,3) = "O"
                    check = 1
                  else
                    do while (check .eq. 0)
                      call random_number(rando)
                      y = int(rando*3)+1
                      z = int(rando*3)+1
                      if ( b(y,z) .eq. "_" ) then
                        b(y,z) = "O"
                        check = 1
                      end if
                    end do
                  end if
                end do
              end if
          end do
        end if


c   "A robot must protect its own existence as long as such
c     protection does not conflict with the first or second law."
        if ( diff .eq. 3 ) then

        end if

      end subroutine Sonny

c Subroutine to check for Victory for either player
      subroutine VCheck(b, rows, cols, victor)
        character, dimension(3,3) :: b
        integer :: r, c
        integer :: rows, cols
        integer :: victor
        logical :: tie
        integer :: vReturn
c   check horizontal conditions


        do r = 1, rows
          c = 1
          if (b(r,c) .ne. "_") then
            if ( b(r,c) .eq. b(r,c+1) .and. b(r,c) .eq. b(r,c+2) ) then
              victor = VReturn(b,r,c)
            end if
          end if
        end do
c   check vertical conditions
        do c = 1, cols
          r = 1
          if (b(r,c) .ne. "_") then
            if (b(r,c) .eq. b(r+1,c) .and. b(r,c) .eq. b(r+2,c)) then
              victor = VReturn(b,r,c)
            end if
          end if
        end do
c   czech diagonals
c     check downward diagonal
        r = 1
        c = 1

        if (b(r,c) .ne. "_") then
          if (b(r,c) .eq. b(r+1,c+1) .and. b(r,c) .eq. b(r+2,c+2)) then
            victor = VReturn(b,r,c)
          end if
        end if
c     check upward diagonal
        if (b(r+2,c) .ne. "_") then
          if (b(r+2,c) .eq. b(r+1,c+1) .and. b(r,c+2) .eq. b(r+2,c))then
            victor = VReturn(b, r+2, c)
          end if
        end if


        if ( victor .eq. 0 ) then
          tie = .true.
          print*, 'Tie check'
          do r = 1, rows
            do c = 1, cols
              print*, 'Board: ', b(r,c)
              if ( b(r,c) .eq. "_" ) then

                tie = .false.
                print*, 'Tie is false', tie
              end if
            end do
          end do
          print*, 'Tie: ', tie
          if ( tie .eqv. .true. ) then

            victor = 3
            print*, 'Victor is', victor
          end if
        end if
      end subroutine VCheck

c Function to return who the victor is
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
        print*, 'Coin: ', coin
        if (coin .lt. 3) then
          write(*,*)
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)
          write(*,*) "  Machines take me by surprise with great frequenc
     +y."
          write(*,*) "                                      - Alan Turin
     +g"
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)
        else if (coin .lt. 5) then
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)
          write(*,*) "  We are not interested in the fact that the brain
     + has"
          write(*,*) "the consistency of cold porridge."
          write(*,*) "                                         - Alan Tu
     +ring"
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)
        else if (coin .lt. 7) then
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)
          write(*,*) "I propose to consider the question,"
          write(*,*) "          'Can machines think?'"
          write(*,*) "                     - Alan Turing"
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)

        else if (coin .lt. 9) then
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)
          write(*,*) "Det. Spooner: Human beings have dreams. Even dogs
     +have dreams, but not"
          write(*,*) "              you, you are just a machine. An imi
     +tation of life."
          write(*,*)"Can a robot write a symphony? Can a robot turn a...
     + canvas into a beautiful masterpiece?"
          write(*,*) "Sonny: Can *you*?"
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)
        else
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*) "Software-industry battles are fought by highly pai
     +d and out-of-shape nerds furiously pounding computer keyboards whi
     +le they guzzle diet Coke."
          write(*,*) "                                - Nathan Myhrvold"
          write(*,*)
          write(*,*) "**************************************************
     +*****"
          write(*,*)
        end if
      end subroutine Turing
