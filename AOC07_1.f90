    program AOC07_1
    use fs_mod
    implicit none
    integer(i8_k) :: totsize, tmp

    call init('..\input.txt')
   ! call dump (root,0)
    tmp = dirsize(root)
    print *, totsize
    
    contains
    
    function dirsize (start) result (lclsize)
    integer(i8_k) :: lclsize
    type(fs_t), target, intent(in) :: start
    type(fs_t), pointer :: curr
    
    lclsize = 0
    
    curr => start
    curr => curr%next
    do while (associated(curr))
        if (.not. associated(curr%down)) then ! file
            lclsize = lclsize + curr%fsize
        else         
            lclsize = lclsize + dirsize(curr%down)
        end if
        curr => curr%next
    end do
    if (lclsize <= 100000) then
        totsize = totsize + lclsize
        write (*,*) start%fname, lclsize
    end if
    
    end function dirsize

    end program AOC07_1

