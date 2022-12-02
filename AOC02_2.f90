    program AOC02_2
    implicit none

    ! Rock=1, Paper=2, Scissors=3
    integer, parameter :: w=6, l=0, d=3
    ! Matrix of score for a play. Dim1=opponent, dim2=player
    integer, parameter :: matrix(3,3) = reshape([ &
        d,w,l, &  ! Opponent plays rock
        l,d,w, &  ! Opponent plays paper
        w,l,d], & ! opponent plays scissors
        [3,3])
    integer, parameter :: result(3) = [l,d,w]
    character :: c1,c2
    integer :: o_plays, p_plays, p_strat
    integer :: score
    integer :: ios
    
    open (unit=1, file='../input.txt', form='formatted', status='old')
    score = 0
    
    do
        read (1,'(A,1X,A)',iostat=ios) c1, c2
        if (ios < 0) exit
        if (c1 == ' ') cycle
        o_plays = (ichar(c1)-ichar('A')) + 1
        p_strat = (ichar(c2)-ichar('X')) + 1
        p_plays = findloc(matrix(:,o_plays),result(p_strat),1)
        score = score + p_plays + matrix(p_plays,o_plays)
    end do
    print *, score
    close (1)

    end program AOC02_2

