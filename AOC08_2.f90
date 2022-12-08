    program AOC08_2

    implicit none

    integer, parameter :: nrows=99,ncols=99
    character(*), parameter :: fname = '..\input.txt'
    integer :: trees(nrows,ncols),row,col,lview,rview,uview,dview,score,maxview,i,j
    
    open (unit=1,file=fname,form='formatted',status='old')
    maxview = 0
    do row=1,nrows
        read (1,'(*(I1))') trees(row,:)
    end do

    do row=2,nrows-1
        do col=2,ncols-1
            lview=1;rview=1;uview=1;dview=1
            ! up
            do i=row-1,1,-1
                if (trees(i,col) >= trees(row,col)) exit
                if (i > 1) uview = uview + 1
            end do
            ! down
            do i=row+1,nrows
                if (trees(i,col) >= trees(row,col)) exit
                if (i < nrows) dview = dview + 1
            end do
            ! left
            do i=col-1,1,-1
                if (trees(row,i) >= trees(row,col)) exit
                if (i > 1) lview = lview + 1
            end do
            ! right
            do i=col+1,ncols
                if (trees(row,i) >= trees(row,col)) exit
               if (i < ncols) rview = rview + 1
            end do
            score = uview*dview*lview*rview
            if (score > maxview) then
                !print *, row, col, score
                maxview = score
            end if
        end do
    end do
    print *, maxview

    end program AOC08_2

