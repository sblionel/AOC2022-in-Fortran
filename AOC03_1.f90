    program AOC03_1
    use, intrinsic :: iso_fortran_env
    implicit none
    
    integer(INT64) :: sack(2)
    character(80) :: line
    character :: item
    character(*), parameter :: alpha='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
    integer :: i, j, k, nitems,priority_sum, ios, spos,epos

    priority_sum = 0
    open (unit=1,file='..\input.txt', form='formatted', status='old')
    outer: do
        sack = 0
        read (1,'(A)',iostat=ios) line
        if (ios /= 0) exit outer
        spos = 1; epos = len_trim(line)/2
        ! For each sack
        do i=1,2
            ! For each item in the line, record its presence
            do j=spos,epos
                item = line(j:j)
                if (item > 'Z') then
                    k = (ichar(item) - ichar('a')) + 1
                else
                    k = (ichar(item) - ichar('A')) + 27
                end if
                sack(i) = ibset(sack(i),k)
            end do
            spos = j; epos = epos*2
            !do j=1,52
            !    if (btest(sack(i),j)) write (*,'(A)',advance='no') alpha(j:j)
            !end do
            !write (*,*)
            
                    
        end do ! Sack number
        
        ! See which items are in both sacks
        sack(1) = iand(sack(1),sack(2))
        do i=1,52
            if (btest(sack(1),i)) priority_sum = priority_sum + i
        end do
    end do outer
    print *, priority_sum
        

    end program AOC03_1

