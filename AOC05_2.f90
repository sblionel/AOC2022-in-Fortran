    program AOC05_2

    implicit none

    integer, parameter :: max_s = 50, n_s = 9
    character :: stacks(-max_s:max_s,n_s),boxes(n_s)
    integer :: curr,top(n_s),bottom,nmove,from,to,ios,i
    character(4) :: x1,x2,x3
    
    stacks = ' '
    top = 0; curr=0
    open (unit=1,file='..\input.txt',form='formatted',status='old')
    
    do curr=1,max_s
        read (1,'(1X,*(A1,3X))') boxes
        if (boxes(1) == '1') exit
        stacks(curr,:) = boxes
        do i=1,n_s
            if ((boxes(i) /= ' ') .and.  (top(i) == 0)) top(i) = curr
        end do
    end do
    bottom = curr - 1
    
    read (1,*) ! Blank line
    
    do
        read (1,*,iostat=ios) x1,nmove,x2,from,x3,to
        if (ios /= 0) exit
        call move_n(nmove,from,to)
    end do
    
    do i=1,n_s
        write (*,'(A)',advance='no') stacks(top(i),i)
    end do
    write (*,*)
    
    close (1)
    
    contains
    
    subroutine move_n (n,src,dest)
    integer, intent(in) :: n,src,dest
    stacks(top(dest)-n:top(dest)-1,dest) = stacks(top(src):top(src)+(n-1),src)
    top(dest) = top(dest) - n
    top(src) = top(src) + n
    end subroutine move_n

    end program AOC05_2

