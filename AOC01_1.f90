    program AOC01_1

    implicit none

    integer, parameter :: ik = selected_int_kind(15)
    integer(ik) :: elf,calories,carrying,max_calories,max_elf,ios
    character(80) :: line
    
    elf = 0
    max_calories = 0
    max_elf = 0
    open (unit=1,file='input.txt',form='formatted',status='old')
    
    outer: do
        elf = elf + 1
        carrying = 0
        inner: do
            read (1,'(A)',iostat=ios) line
            if (line == '' .or. ios < 0) exit inner
            read (line,*) calories
            carrying = carrying + calories
        end do inner
        if (carrying > max_calories) then
            max_calories = carrying
            max_elf = elf
        end if
        if (ios < 0) exit outer
    end do outer
    print *, "Elf #",max_elf," carries ",max_calories," calories"
    close (1)

    end program AOC01_1