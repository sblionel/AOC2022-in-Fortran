    program AOC10_1

    implicit none

    integer :: operand,x,cycle,delay,delayval,sigstrength,ios
    character(4) :: instr
    character(20) :: line
    logical :: dodelay
    
    open (unit=1,file='..\input.txt',form='formatted',status='old')
    x = 1; cycle=1; sigstrength=0; delay=0; dodelay=.false.
    
    do
        if (delay == 0) then
            read (1,'(A)',iostat=ios) line
            if (ios /= 0) exit
            select case (line(1:4))
            case('noop')
                ! do nothing
            case('addx')
                read (line,*) instr,operand
                delayval = x + operand
                delay = 1
            end select
        else
            delay = delay - 1
            dodelay = (delay == 0)
        end if
        if (findloc([20,60,100,140,180,220],cycle,1) > 0) then
            sigstrength = sigstrength + (cycle * x)
        end if
        if (dodelay) then
            x = delayval
            dodelay = .false.
        end if     
        cycle = cycle + 1
    end do
    print *, sigstrength
    end program AOC10_1

