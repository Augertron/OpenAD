MODULE memory

public :: keeper, getSpace

 integer,  pointer :: keeper

 interface getSpace
 module procedure getSpace_i
 end interface

CONTAINS

  subroutine getSpace_i(p) 
   integer,  pointer :: p 
   allocate (p)
   keeper=>p
  end subroutine

end module


subroutine forgetSpace(p)
   integer,  pointer :: p
   integer,  target :: x
   p=5
   p=>x
   p=4
   print *, p
end subroutine

program unnamed

use memory

interface forgetSpace
  subroutine forgetSpace(p)
   integer,  pointer :: p
  end subroutine
end interface

  integer,  pointer :: p

  call getSpace(p)

  call forgetSpace(p)

  print *, p  

end program
