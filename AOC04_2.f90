!  AOC04_2.f90 
    program AOC04_2

    implicit none

    integer :: r1(2),r2(2),ios,i,overlaps, overlap
    character(80) :: line
    open (unit=1,file='../input.txt',form='formatted', status='old')
    overlaps = 0
    do
        read (1,'(A)',iostat=ios) line
        if (ios /= 0) exit
        do i=1,len_trim(line)
            if (line(i:i)=='-') line(i:i) = ' '
        end do
        read (line,*) r1,r2
        overlap = -1
        if ((r1(1) <= r2(1)) .and. (r2(1) <= r1(2))) then
            overlap = (min(r1(2),r2(2))-max(r1(1),r2(1)))
        else if ((r2(1) <= r1(1)) .and. (r1(1) <= r2(2))) then
            overlap =  (min(r1(2),r2(2))-max(r1(1),r2(1)))
        end if            
        if (overlap >= 0) overlaps = overlaps + 1
    end do
    print *, overlaps

    end program AOC04_2

