module am 

  public :: a, b, c, foo

  integer a,b,c

  interface foo
    module procedure foo_impl
  end interface

contains

  subroutine foo_impl()
    print *,a
  end subroutine 

end module


program main
  use am, only: b,c,foo
  implicit none
  integer a
  c=1
  a=2
  call foo()
end program
