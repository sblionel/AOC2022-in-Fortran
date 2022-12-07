module fs_mod
    implicit none
    integer, parameter :: i8_k = selected_int_kind(15)
    
    type fs_t
        type(fs_t), pointer :: up => null() ! Directories only
        type(fs_t), pointer :: down => null() ! Directories only
        type(fs_t), pointer :: next => null()
        integer(i8_k) :: fsize = 0  
        character(16) :: fname
    end type fs_t
    type(fs_t), target :: root
    type(fs_t), pointer :: curdir
    character(80) :: line
    integer :: ios
    
    contains
    
    subroutine init (input_fname)
    character(*), intent(in) :: input_fname
    type(fs_t), pointer :: tmp, node
    
    root%fname = '/'
    
    open (unit=1, file=input_fname, form='formatted', status='old')
    read (1,'(A)', iostat=ios) line
    do while (ios == 0)
        ! At this point, line contains a command
        select case (line(1:4))
        case ('$ cd')
            if (line(6:7) == '/ ') then
                curdir => root 
            else if (line(6:7) == '..') then
                if (.not. associated(curdir%up)) error stop "Can't go up"
                curdir => curdir%up
            else
                tmp => lookup_name (curdir,line(6:))    
                if (.not. associated(tmp%down)) error stop "Dir "//trim(line(6:))//" does not exist"
                curdir => tmp%down
            end if
            read (1,'(A)',iostat=ios) line
        case ('$ ls')
            call list_dir (curdir)
        case default
            error stop "Unknown command "//trim(line)
        end select
    end do
    end subroutine init
    
    subroutine list_dir (start)
    type(fs_t), pointer, intent(in) :: start
    type(fs_t), pointer :: node, tmp
    character(16) :: t1, t2
    
    read (1,'(A)', iostat=ios) line
    do while (ios == 0)
        if (line(1:1) == '$') exit
        read (line,*) t1,t2 ! Space delimiter
        if (t1 == 'dir') then
            tmp => lookup_name(start, t2)
            if (.not. associated(tmp%up)) then ! Newly created dir
                allocate (node)
                tmp%up => start
                node%fname = tmp%fname
                node%down => tmp
                node%next => start%next
                start%next => node
            end if ! If existing, don't re-add it
        else
            ! File
            tmp => lookup_name (start, t2)
            if (tmp%fsize == 0) then ! Newly created
                read (t1,*) tmp%fsize
                tmp%next => start%next
                start%next => tmp
            end if
        end if
        read (1,'(A)', iostat=ios) line
    end do
    end subroutine list_dir
        
    
    function lookup_name (start, name)
    type(fs_t), pointer :: lookup_name
    type(fs_t), pointer, intent(in) :: start
    character(*), intent(in) :: name
    type(fs_t), pointer :: curr
    character(80) :: tmpline
    
    curr => start%next
    do while (associated(curr))
        if (curr%fname == name) then
            lookup_name => curr
            return
        end if
        curr => curr%next
    end do
    
    ! Not found, create a new one
    allocate (lookup_name)
    lookup_name%fname = name
    
    end function lookup_name
    
    subroutine dump (start, level)
    type(fs_t), target, intent(in) :: start
    integer, intent(in) :: level
    type(fs_t), pointer :: curr
    integer :: curr_level
    
    curr => start
    curr_level = level + 1

    write(*, '(A,"- ",A," (dir)")') repeat(' ',curr_level), trim(curr%fname)
    curr => curr%next
    do while (associated(curr))
        if (.not. associated(curr%down)) then ! file
            write (*,'(A,"- ",A," (file, size=",I0,")")') repeat('  ',curr_level),trim(curr%fname),curr%fsize
        else         
            call dump(curr%down,curr_level)
        end if
        curr => curr%next
    end do
    end subroutine dump
        
    
    
end module fs_mod
    