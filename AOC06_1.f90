    program AOC06_1

    implicit none

    character(:), allocatable :: input
    integer, parameter :: a = ichar('a')
    integer :: counts(ichar('a'):ichar('z')), curpos, i
    character :: c
    
    allocate(character(5000) :: input)
    open (unit=1, file='..\input.txt', form='formatted', status='old')
    read (1,'(A)') input
    
    mainloop: do curpos = 1,len_trim(input)-3
        counts = 0
        do i=0,3
            c = input(curpos+i:curpos+i)
            if (counts(ichar(c)) > 0) cycle mainloop
            counts(ichar(c)) = 1
        end do
        exit mainloop
    end do mainloop
    
    print *, curpos+3


    end program AOC06_1

