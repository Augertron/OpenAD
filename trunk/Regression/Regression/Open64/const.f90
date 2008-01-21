	program const
	double precision:: talpha=2.D-4
	double precision:: tbeta=1.234567D5
	double precision, parameter :: PI=3.14159265358979323844D0
	print*, talpha, tbeta
	write(*,'(A)') '   3.14159265358979323844'
	write(*,'(EN26.16E3)') PI
	end program
