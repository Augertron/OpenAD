	program arraypointer
	  double precision, dimension(3), target :: x = (/1.0,2.0,3.0/)
	  double precision, dimension(:), pointer :: p
	  allocate(p(3))
	  p(1)=0.1
	  p(2)=0.2
	  p(3)=0.3
 	  print *, p
	  p=>x ! now we lost the name for the  space allocated to 3
	  print *, p
	end program
