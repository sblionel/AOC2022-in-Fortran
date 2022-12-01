    program AOC01_2

    implicit none

    integer, parameter :: ik = selected_int_kind(15)
    integer(ik) :: elf,calories,carrying,top(3),ios
    character(80) :: line
    
    elf = 0
    top = 0
    open (unit=1,file='../input.txt',form='formatted',status='old')
    
    outer: do
        elf = elf + 1
        carrying = 0
        inner: do
            read (1,'(A)',iostat=ios) line
            if (line == '' .or. ios < 0) exit inner
            read (line,*) calories
            carrying = carrying + calories
        end do inner
        if (carrying > top(1)) then
            if (carrying > top(3)) then
                top(1) = top(2)
                top(2) = top(3)
                top(3) = carrying
            else if (carrying > top(2)) then
                top(1) = top(2)
                top(2) = carrying
            else
                top(1) = carrying
            end if     
        end if
        if (ios < 0) exit outer
    end do outer
    print *, sum(top)
    close (1)

    end program AOC01_2