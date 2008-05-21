	subroutine foo (i)
	integer*8 i
	print *, i
	end subroutine

	program const
	double precision:: talpha=2.D-4
	double precision:: tbeta=1.234567D5
	double precision, parameter :: PI=3.14159265358979323844D0
	integer*8, parameter:: i=999999999
	print*, talpha, tbeta
	write(*,'(A)') '   3.14159265358979323844'
	write(*,'(EN26.16E3)') PI
	call foo(i)
	call foo(9)
	end program
