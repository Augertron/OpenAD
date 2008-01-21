	subroutine head() 
	  double precision :: x
	  double precision, target:: y
	  double precision, pointer :: p
	  p=>y
	  p=x
	end subroutine
