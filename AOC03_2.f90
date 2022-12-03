    program AOC03_2

     use, intrinsic :: iso_fortran_env
    implicit none
    
    integer(INT64) :: sack(3)
    character(80) :: line
    character :: item
    character(*), parameter :: alpha='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
    integer :: i, j, k, nitems,priority_sum, ios, spos,epos

    priority_sum = 0
    open (unit=1,file='..\input.txt', form='formatted', status='old')
    outer: do
        sack = 0
        ! For each sack
        do i=1,3
            read (1,'(A)',iostat=ios) line
            if (ios /= 0) exit outer
            ! For each item in the line, record its presence
            do j=1,len_trim(line)
                item = line(j:j)
                if (item > 'Z') then
                    k = (ichar(item) - ichar('a')) + 1
                else
                    k = (ichar(item) - ichar('A')) + 27
                end if
                sack(i) = ibset(sack(i),k)
            end do                    
        end do ! Sack number
        
        ! See which items are in all three sacks
        sack(1) = iall(sack)
        do i=1,52
            if (btest(sack(1),i)) then
                priority_sum = priority_sum + i
                print *, alpha(i:i)
            end if
            
        end do
    end do outer
    print *, priority_sum


    end program AOC03_2

