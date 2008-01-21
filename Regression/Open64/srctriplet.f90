	subroutine head(i,j) 
	  double precision x(2)
	  integer i,j
	  x(i::1)=x(:j:1)
	end subroutine
