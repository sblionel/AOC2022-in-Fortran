    program AOC09_1

    implicit none

    integer :: grid(-300:300,-300:300),hx,hy,tx,ty,visited,move,ios
    character :: dir
    
    open (unit=1,file='..\input.txt',form='formatted',status='old')
    grid = 0; hx=0; hy=0; tx=0;ty=0;visited=1
    grid(0,0) = 1
    
    do
        read (1,*,iostat=ios) dir,move
        if (ios /= 0) exit
        select case (dir)
        case('U')
            hy=hy+move
        case('D')
            hy=hy-move
        case('L')
            hx=hx-move
        case('R')
            hx=hx+move
        end select
        do while ((abs(hx-tx)>1) .or. (abs(hy-ty)>1))
            if (hx /= tx) tx = tx + sign(1,hx-tx)
            if (hy /= ty) ty = ty + sign(1,hy-ty)
            if (grid(tx,ty) == 0) then
                grid(tx,ty) = 1
                visited = visited+1
            end if
        end do
    end do
    print *, visited
    end program AOC09_1

