    program AOC09_2

    implicit none

    integer :: grid(-300:300,-300:300),rx(0:9),ry(0:9),visited,move,ios,i,k
    character :: dir
    
    open (unit=1,file='..\input.txt',form='formatted',status='old')
    grid = 0; rx=0; ry=0;visited=1
    grid(0,0) = 1
    
    do
        read (1,*,iostat=ios) dir,move
        if (ios /= 0) exit
        do i=1,move
            select case (dir)
            case('U')
                ry(0)=ry(0)+1
            case('D')
                ry(0)=ry(0)-1
            case('L')
                rx(0)=rx(0)-1
            case('R')
                rx(0)=rx(0)+1
            end select
            do k=1,9
                do while ((abs(rx(k)-rx(k-1))>1) .or. (abs(ry(k)-ry(k-1))>1))
                    if (rx(k) /= rx(k-1)) rx(k) = rx(k) + sign(1,rx(k-1)-rx(k))
                    if (ry(k) /= ry(k-1)) ry(k) = ry(k) + sign(1,ry(k-1)-ry(k))
                    if ((k == 9) .and. grid(ry(9),rx(9)) == 0) then
                        grid(ry(9),rx(9)) = 1
                        visited = visited+1
                    end if
                end do
            end do       
        end do
    end do
    print *, visited
    end program AOC09_2

