subroutine mergefiles
    !Stolen from 
    !https://stackoverflow.com/questions/42983557/inquire-to-find-files-with-certain-extension-in-fortran
    !And modified for my use
    use iso_fortran_env
    implicit none
    double precision :: x1, x2, x3, x4, x5, x7, x8, x9, x10
    logical :: LOG_var
    character(len=*), parameter :: ls_file = '/tmp/my_ls.tmp'
    integer :: u, ios, i, io, j, n, myline
    character(len=30) :: filename
    open(2,file='Complete.dat',form='formatted')
    call execute_command_line('ls -1 > '//ls_file, wait=.TRUE., exitstat=ios)
    if (ios /= 0) stop "Unable to get listing"

    open(newunit=u, file=ls_file, iostat=ios, status="old", action="read")
    if ( ios /= 0 ) stop "Error opening listing file "


    !allocate( x(n) ,y(n) )
    do
        read(u, *, iostat=ios) filename
        if (is_iostat_end(ios)) exit
        if (ios /= 0) STOP "Unexpected error while reading listing file"
        if ((index(filename, "file") > 0).and. (index(filename, ".dat") > 0)) then
            !print *, filename
            open(1,file=filename,form='UNFORMATTED')
            !Getting length of datafile
            n = 0
            DO
            READ(1,iostat=io)
                IF (io/=0) EXIT
                n = n + 1
            END DO
            REWIND(1)                                                                                                           
	    !print*, n
            !Putting datafile into x line for line and trasfer to other file
            DO i =1,n
                READ(1) myline, x1, x2, x3, x4, x5, LOG_var, x7, x8, x9, x10
		!WRITE(6,'(I4,x,F9.5,x,F8.5,x,F8.5,x,E10.5,x,E19.5,x,L,x,F8.5,x,F8.5,x,F15.5,x,E19.7)') &
                !myline, x1, x2, x3, x4, x5, LOG_var,x7, x8, x9, x10
                WRITE(2,'(I4,x,F9.5,x,F8.5,x,F8.5,x,E10.5,x,E19.5,x,L,x,F8.5,x,F8.5,x,F15.5,x,E19.7)') &
                myline, x1, x2, x3, x4, x5, LOG_var, x7, x8, x9, x10
	    ENDDO
            !print *,'HI'
            close(1)                                                                                                            
        end if
    end do
    !print *, myline, x1, x2, x3, x4, x5, LOG_var, x7, x8, x9
    close(u)
    call execute_command_line('rm '//ls_file, wait=.FALSE.)
    close(2)

end subroutine mergefiles
