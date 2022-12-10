    program AOC10_2

    implicit none

    integer :: operand,x,cycle,delay,delayval,ios,pixelpos
    character(4) :: instr
    character(20) :: line
    logical :: dodelay
    character :: scanline(0:39)
    
    open (unit=1,file='..\input.txt',form='formatted',status='old')
    x = 1; cycle=1; delay=0; dodelay=.false.
    scanline = ' '
    
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
        pixelpos= mod(cycle-1,40)
        if (abs(x-pixelpos) <= 1) scanline(pixelpos) = '#'
        if (pixelpos == 39) then
            write (*,'(40A1)') scanline
            scanline = ' '
        end if
            
        if (dodelay) then
            x = delayval
            dodelay = .false.
        end if     
        cycle = cycle + 1
    end do

    end program AOC10_2

