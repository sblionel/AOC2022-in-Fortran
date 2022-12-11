    program AOC11_1

    implicit none
    
    integer, parameter :: max_monkey = 7
    character(*), parameter :: input_fname = "..\input.txt"

    type item_t
        type(item_t), pointer :: prev => null()
        type(item_t), pointer :: next => null()
        integer :: worry = 0
    end type item_t
    
    integer, parameter :: op_add = 1, op_mul = 2
    type monkey_t
        type(item_t), pointer :: first => null()
        type(item_t), pointer :: last => null()
        integer :: operator = 0
        integer :: operand = 0
        integer :: testdiv = 0
        integer :: iftrue = 0
        integer :: iffalse = 0
        integer :: inspected = 0
    end type monkey_t
        
    type(monkey_t) :: monkeys(0:max_monkey)
    
    integer :: i, round, operand, topcount(2)
    type(item_t), pointer :: curr_item,this_item
    
    open (unit=1,file=input_fname,form='formatted',status='old')
    
    do i=0,max_monkey
        call get_monkey(i)
    end do
    
    
    do round=1,20
        do i=0,max_monkey
            curr_item => monkeys(i)%first
            do while (associated(curr_item))
                monkeys(i)%inspected = monkeys(i)%inspected + 1
                this_item => curr_item
                curr_item => curr_item%next
                if (monkeys(i)%operand == -1) then
                    operand = this_item%worry
                else
                    operand = monkeys(i)%operand
                end if
                if (monkeys(i)%operator == op_add) then
                    this_item%worry = this_item%worry + operand
                else
                    this_item%worry = this_item%worry * operand
                end if
                this_item%worry = this_item%worry / 3 ! Truncates
                if (mod(this_item%worry,monkeys(i)%testdiv) == 0) then
                    call throw_item (this_item,monkeys(i),monkeys(monkeys(i)%iftrue))
                else
                    call throw_item (this_item,monkeys(i),monkeys(monkeys(i)%iffalse))
                end if
            end do
        end do
    end do
    
    topcount = 0
    do i=0,max_monkey
        if (monkeys(i)%inspected > topcount(1)) then
            topcount(2) = topcount(1)
            topcount(1) = monkeys(i)%inspected
        else if (monkeys(i)%inspected > topcount(2)) then
            topcount(2) = monkeys(i)%inspected
        end if
    end do
    print *, "Monkey business = ",topcount(1)*topcount(2)
    
    
    contains
    
    subroutine get_monkey (n)
    use, intrinsic :: iso_fortran_env
    integer, intent(in) :: n
    character(4) :: str1,str2
    type(item_t), pointer :: curr_item
    integer :: worry, ios
    character(80) :: line
    
    read (1,*) ! Monkey n:
    read (1,'(A16)',advance='no') line ! Starting items:
    do
        read (1,'(1X,I3)',advance='no',iostat=ios) worry
        if (ios /= 0) exit
        allocate (curr_item)
        curr_item%worry = worry
        if (associated(monkeys(n)%first)) then ! Not the first item
            curr_item%prev => monkeys(n)%last
            monkeys(n)%last%next => curr_item
            monkeys(n)%last => curr_item
        else ! First item
            monkeys(n)%first => curr_item
            monkeys(n)%last => curr_item
        end if
    end do
    
    read (1,'(A23)',advance='no') str1 ! Operation: new = old
    read (1,*) str1,str2
    if (str1 == '+') then
        monkeys(n)%operator = op_add
    else if (str1 == '*') then
        monkeys(n)%operator = op_mul
    else
        error stop "Unknown operation"
    end if
    if (str2 == 'old') then
        monkeys(n)%operand = -1
    else
        read (str2,*) monkeys(n)%operand
    end if
    
    read (1,'(T22,I5)') monkeys(n)%testdiv ! Test: divisible by
    read (1,'(T30,I5)') monkeys(n)%iftrue ! If true: throw to monkey
    read (1,'(T31,I5)') monkeys(n)%iffalse ! If false: throw to monkey
    read (1,*,iostat=ios) ! Blank line or EOF
    end subroutine get_monkey
    
    subroutine throw_item (item,from,to)
    type(item_t), pointer, intent(inout) :: item
    type(monkey_t), intent(inout) :: from, to
    
    ! Remove from throwing monkey
    if (associated(item,from%first)) then
        from%first => item%next
    else
        item%prev%next => item%next
    end if
    if (associated(item,from%last)) then
        from%last => item%prev
    else
        item%next%prev => item%prev
    end if
    ! Add to receiving monkey
    item%prev => to%last
    item%next => null()
    if (associated(to%last)) to%last%next => item
    to%last => item
    if (.not. associated(to%first)) to%first => item
    end subroutine throw_item
    
    subroutine dump
    type(item_t), pointer :: curr_item
    integer :: i
    character :: ops(2) = ['+','*']
    do i = 0,max_monkey
        write (*,'("Monkey ",I0,":")') i
        write (*,'("  Starting items:")',advance='no')
        curr_item => monkeys(i)%first
        do while (associated(curr_item))
            write (*,'(1X,I0)',advance='no') curr_item%worry
            curr_item => curr_item%next
            if (associated(curr_item)) write (*,'(",")', advance='no')
        end do
        write (*,*)
        write (*,'("  Operation: new = old ",A1)',advance='no') ops(monkeys(i)%operator)
        if (monkeys(i)%operand >= 0) then
            write (*,'(1X,I0)') monkeys(i)%operand
        else
            write (*,'(" old")')
        end if
        write (*,*) " Test: divisible by ",monkeys(i)%testdiv
        write (*,*) "   If true: throw to monkey ",monkeys(i)%iftrue
        write (*,*) "   If false: throw to monkey",monkeys(i)%iffalse
        write (*,*) " Inspected: ",monkeys(i)%inspected
    end do
    end subroutine dump
    

    end program AOC11_1

