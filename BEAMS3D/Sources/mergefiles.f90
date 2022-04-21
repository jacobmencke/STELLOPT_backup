subroutine mergefiles
    !And modified for my use
    implicit none
    double precision :: x1, x2, x3, x4, x5, x7, x8, x9, x10
    logical :: LOG_var, file_exists
    character(len=*), parameter :: ls_file = '/tmp/my_ls.tmp'
    integer :: u, ios, i, io, j, n, myline, k
    character(len=10) :: file_id
    character(len=30) :: filename
    open(2,file='Complete.dat',form='formatted')


    !allocate( x(n) ,y(n) )
    !I basically just try the first 10 million mylinestarts and ask if the dat file exist
    do k=1,10000000!10 million
	write(file_id, '(i0)')
        filename='file' // trim(adjustl(file_id))
        INQUIRE(trim(filename // '.dat'),EXIST=file_exists)
        if file_exists then
            !print *, filename
            !Getting length of datafile
	    open(3,file=trim(filename// '.txt'),form='UNFORMATTED')
	    READ(3) n
	    close(3)
            open(1,file=trim(filename // '.dat'),form='UNFORMATTED')
            !Putting datafile into x line for line and trasfer to other file
            DO i =1,n
                READ(1) myline, x1, x2, x3, x4, x5, LOG_var, x7, x8, x9, x10
		!WRITE(6,'(I4,x,F9.5,x,F8.5,x,F8.5,x,E10.5,x,E19.5,x,L,x,F8.5,x,F8.5,x,F15.5,x,E19.7)') &
                !myline, x1, x2, x3, x4, x5, LOG_var,x7, x8, x9, x10
                WRITE(2,'(I4,x,F9.5,x,F8.5,x,F8.5,x,E10.5,x,E19.5,x,L,x,F8.5,x,F8.5,x,F15.5,x,E19.7)') &
                myline, x1, x2, x3, x4, x5, LOG_var, x7, x8, x9, x10
	    ENDDO
            
            close(1)                                                                                                            
        end if
    end do
    !print *, myline, x1, x2, x3, x4, x5, LOG_var, x7, x8, x9
    close(2)

end subroutine mergefiles
