module am 

  public :: a, b, foo

  integer a,b

  interface foo
    module procedure foo_impl
  end interface

contains

  subroutine foo_impl()
    print *,a
  end subroutine 

end module


program main
  use am, only: c=>a, foo
  implicit none
  integer a
  c=1
  a=2
  call foo()
end program
