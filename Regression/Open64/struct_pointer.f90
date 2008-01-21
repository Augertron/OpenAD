	module myTypeModule

        implicit none
        private
        public :: myType
	
        type myType
	sequence
	double precision :: field1 
	end type myType
	end module

	subroutine head() 
	  use myTypeModule
	  type(myType) :: typed_x
	  type(mytype), target:: typed_y
	  type(mytype), pointer :: typed_p
	  typed_p=>typed_y
	  typed_p%field1=typed_x%field1
	end subroutine
