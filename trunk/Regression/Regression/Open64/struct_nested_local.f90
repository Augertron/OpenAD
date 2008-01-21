       module myNestedTypeM
       public :: myNestedType
       type myNestedType
       sequence
       double precision :: nestedField 
       end type
       end module

       module myTypeM
       use myNestedTypeM
       public :: myType
       type myType
       type(myNestedType) :: field 
       end type
       end module
      
       subroutine head() 
       use myTypeM
       double precision  x
       type(myType) :: typed_y
       typed_y%field%nestedField=x
       end subroutine
