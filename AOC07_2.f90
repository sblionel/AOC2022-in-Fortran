program AOC07_2
    use fs_mod
    implicit none
    integer(i8_k) :: totsize, free_space, minsize

    call init('..\input.txt')
   ! call dump (root,0)
    totsize = dirsize(root, 0_i8_k)
    free_space = 70000000_i8_k - totsize
    minsize = huge(minsize)
    totsize = dirsize(root,free_space)
    
    
    contains
    
    function dirsize (start, free_space) result (lclsize)
    integer(i8_k) :: lclsize
    type(fs_t), target, intent(in) :: start
    integer(i8_k), intent(in) :: free_space
    type(fs_t), pointer :: curr
    
    lclsize = 0
    
    curr => start
    curr => curr%next
    do while (associated(curr))
        if (.not. associated(curr%down)) then ! file
            lclsize = lclsize + curr%fsize
        else         
            lclsize = lclsize + dirsize(curr%down, free_space)
        end if
        curr => curr%next
    end do
    if ((free_space > 0) .and. ((free_space+lclsize) > 30000000) .and.&
        (lclsize < minsize)) then
        minsize = lclsize
        write (*,*) trim(start%fname), ' ',lclsize
    end if
    
    end function dirsize

    end program AOC07_2

