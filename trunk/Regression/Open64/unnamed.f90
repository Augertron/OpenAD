MODULE memory

public :: keeper, getSpace

 integer, dimension(:), pointer :: keeper

 interface getSpace
 module procedure getSpace_i
 end interface

CONTAINS

  subroutine getSpace_i(p) 
   integer, dimension(:), pointer :: p 
   allocate (p(2))
   keeper=>p
  end subroutine

end module


subroutine forgetSpace(p)
   integer, dimension(:), pointer :: p
   integer, dimension(2), target :: x
   p(1)=5
   p=>x
   p(1)=4
   print *, p
end subroutine

program unnamed

use memory

interface forgetSpace
  subroutine forgetSpace(p)
   integer, dimension(:), pointer :: p
  end subroutine
end interface

  integer, dimension(:), pointer :: p

  call getSpace(p)

  call forgetSpace(p)

  print *, p  

end program
