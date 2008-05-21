	subroutine foo(a,n)
	integer n
	integer, dimension (n+n) :: a
	print *,a
	end 

	subroutine bar(a)
	integer, dimension(:) :: a
	print *,a(:)
	end 

	program test
	interface 
	subroutine bar(a)
        integer, dimension(:) :: a
	end subroutine
	end interface
	integer, parameter :: n1=2, n2=4
	integer, dimension(n1) :: a1 
	integer, dimension(n2) :: a2
	call foo(a1,1)
	call foo(a2,2)
	call bar(a1)
	call bar(a2)
	end program
