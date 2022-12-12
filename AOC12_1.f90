
    program AOC12_1

    implicit none

    character(*), parameter :: inpfile = '..\input.txt'
    integer :: xsize, ysize, curx, cury, i, ios, candidate(2), dest(2)
    character(512) :: line
    integer, allocatable :: grid(:,:), costs(:,:)
    logical, allocatable ::  notinset(:,:)
    integer, parameter :: adjx(4) = [-1,1,0,0], adjy(4)=[0,0,1,-1]
    
    open (unit=1,file=inpfile,form='formatted',status='old')
    read (1,'(A)') line
    xsize = len_trim(line)
    ysize = 0; ios = 0
    do while (ios == 0)
        ysize = ysize + 1
        read (1,'(A)',iostat=ios) line
    end do
    allocate (grid(xsize,ysize),costs(xsize,ysize),notinset(xsize,ysize))
    costs = huge(0)
    notinset = .true.
    rewind (1)
    do cury=1,ysize
        read (1,'(A)') line
        do curx=1,xsize
            if (line(curx:curx) == 'S') then
                costs(curx,cury) = 0
                grid(curx,cury) = 0
            else if (line(curx:curx) == 'E') then
                dest = [curx,cury]
                grid(curx,cury) = 25
            else
                grid(curx,cury) = ichar(line(curx:curx))-ichar('a')
            end if
        end do
    end do
    
    do
        candidate = minloc(costs,mask=notinset)
        if (all(candidate == dest)) exit
        curx = candidate(1)
        cury = candidate(2)
        notinset(curx,cury) = .false.
        do i=1,4
            if ((curx+adjx(i) < 1) .or. (curx+adjx(i) > xsize)) cycle
            if ((cury+adjy(i) < 1) .or. (cury+adjy(i) > ysize)) cycle
            if (notinset(curx+adjx(i),cury+adjy(i))) then
                block
                    integer :: newcost, newsteps
                    if (grid(curx+adjx(i),cury+adjy(i))-grid(curx,cury) <= 1) then
                       newcost = costs(curx,cury) + 1
                       if (newcost < costs(curx+adjx(i),cury+adjy(i))) then
                           costs(curx+adjx(i),cury+adjy(i)) = newcost
                       end if
                    end if
                end block
            end if       
        end do
    end do
    print *, costs(dest(1),dest(2))

    end program AOC12_1

