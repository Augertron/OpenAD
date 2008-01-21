	module myTypeModule

        implicit none
        private
        public :: myType
	
        type myType
	sequence
	double precision :: field1 
	double precision :: field2=0.0
	end type myType
	end module

	module myTypeModule2

        use myTypeModule

        implicit none
        private
        public :: myType2
	
        type myType2
	sequence
	type(mytype), dimension(2) :: field1 
	type(myType) :: field2=myType(0.0,0.0)
	end type myType2
	end module

	subroutine head(x) 
	
	  use myTypeModule2

	  double precision x
	  type(myType2) :: typed_x
	  typed_x%field2%field1 = x
	  typed_x%field1(1)%field2 = x
	end subroutine
