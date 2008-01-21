	module myTypeModule

        implicit none
        private
        public :: myType
	
        type myType
	sequence
	double precision :: field1 
	end type myType
	end module

	subroutine head(x, typed_y) 
	  use myTypeModule
	  double precision  x
	  type(myType) :: typed_y
	  typed_y%field1=x
	end subroutine
