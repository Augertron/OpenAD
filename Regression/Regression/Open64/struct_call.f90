	module myTypeModule

        implicit none
        private
        public :: myType
	
        type myType
	sequence
	double precision :: f1 
	end type myType
	end module

	subroutine head(x, y) 
	  use myTypeModule
	  double precision  x
	  type(myType), intent(in) :: y
	  x=y%f1
	end subroutine

	program test 
	  use myTypeModule
	  double precision x
	  type(myType), target :: p 
 	  p%f1=2.0
	  call head(x,p)
        end program
