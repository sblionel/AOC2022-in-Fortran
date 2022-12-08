    program AOC08_1

    implicit none

    integer, parameter :: nrows=99,ncols=99
    character(*), parameter :: fname = '..\input.txt'
    integer :: trees(nrows,ncols),rowmax,colmax,idxmax,row,col,visible
    logical :: seen(nrows,ncols)
    
    open (unit=1,file=fname,form='formatted',status='old')
    rowmax = 0; colmax = 0; visible=(2*nrows)+((2*ncols)-4)
    seen = .false.
    do row=1,nrows
        read (1,'(*(I1))') trees(row,:)
    end do
    do row=2,nrows-1
        rowmax = trees(row,1)
        idxmax = 1
        do col=2,ncols-1
            if (trees(row,col) > rowmax) then
                visible = visible + 1
                rowmax = trees(row,col)
                seen(row,col) = .true.
                idxmax = col
                if (rowmax == 9) exit
            end if
        end do
        rowmax=trees(row,ncols)
        do col=ncols-1,idxmax+1,-1
            if (trees(row,col) > rowmax) then
                visible = visible + 1
                rowmax = trees(row,col)
                seen(row,col) = .true.
                if (rowmax == 9) exit
            end if
        end do
    end do
  
    do col=2,ncols-1
        colmax = trees(1,col)
        idxmax = 1
        do row=2,nrows-1
            if (trees(row,col) > colmax) then
                if (.not. seen(row,col)) then
                    visible = visible + 1
                    seen(row,col) = .true.
                end if
                colmax = trees(row,col)
                idxmax = row
                if (colmax == 9) exit
            end if
        end do
        colmax=trees(nrows,col)
        do row=nrows-1,idxmax+1,-1
            if (trees(row,col) > colmax) then
                if (.not. seen(row,col)) then
                    visible = visible + 1
                    seen(row,col) = .true.
                end if
                colmax = trees(row,col)
                if (colmax == 9) exit
            end if
        end do
    end do
    print *, visible

    end program AOC08_1