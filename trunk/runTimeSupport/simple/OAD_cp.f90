!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
module OAD_cp

  use OAD_active

  implicit none

  private

  integer, parameter :: store_increase=20000

  public ::  &
       cp_store_real_scalar, & 
       cp_store_real_vector, &
       cp_store_p_real_vector, & 
       cp_store_int_scalar, cp_store_int_vector, &
       cp_store_string_scalar, cp_store_bool_scalar 

  integer, parameter, public :: string_length=80

  interface cp_store_real_scalar
     module procedure cp_store_real_scalar_impl
  end interface

  interface cp_store_real_vector
     module procedure cp_store_real_vector_impl
  end interface

  interface cp_store_p_real_vector
     module procedure cp_store_p_real_vector_impl
  end interface

  interface cp_store_int_scalar
     module procedure cp_store_int_scalar_impl
  end interface

  interface cp_store_int_vector
     module procedure cp_store_int_vector_impl
  end interface

  interface cp_store_string_scalar
     module procedure cp_store_string_scalar_impl
  end interface

  interface cp_store_bool_scalar
     module procedure cp_store_bool_scalar_impl
  end interface

contains

  subroutine cp_store_real_scalar_impl(x,s,c,a)
    ! store x in stack s of allocated size a in current position c
    implicit none
    double precision, intent(in) :: x
    integer :: c,a
    double precision, dimension(:), allocatable :: s
    ! temp array for potential reallocation
    double precision, dimension(:), allocatable :: temp
    if(a<c+1) then 
       if (a>0) then 
          allocate(temp(a))
          temp=s
          deallocate(s)
       end if
       allocate(s(a+store_increase))
       if (a>0) then 
          s(1:a) = temp
          deallocate(temp)
       end if
       a=a+store_increase
    end if
    c=c+1
!    write(*,'(A,EN26.16E3,I)') "store(s)  ", x, c
    s(c)=x
  end subroutine cp_store_real_scalar_impl

  subroutine cp_store_real_vector_impl(x,n,s,c,a)
    ! store x of size n in stack s of allocated size a in current position c
    implicit none
    type(active), dimension(:), intent(in) :: x
    integer, intent(in) :: n
    integer :: c,a,i
    double precision,dimension(:), allocatable :: s
    ! temp array for potential reallocation
    double precision, dimension(:), allocatable :: temp
    if(a<c+n) then 
       if (a>0) then 
          allocate(temp(a))
          temp=s
          deallocate(s)
       end if
       allocate(s(a+store_increase))
       if (a>0) then 
          s(1:a) = temp
          deallocate(temp)
       end if
       a=a+store_increase
    end if
    do i=1,n
!       write(*,'(A,EN26.16E3,I)') "store(v)  ", x(i)%v, c+i
       s(c+i)=x(i)%v
    end do
    c=c+n
  end subroutine cp_store_real_vector_impl

  subroutine cp_store_p_real_vector_impl(x,n,s,c,a)
    ! store x of size n in stack s of allocated size a in current position c
    implicit none
    double precision,dimension(:), intent(in) :: x
    integer, intent(in) :: n
    integer :: c,a,i
    double precision,dimension(:), allocatable :: s
    ! temp array for potential reallocation
    double precision, dimension(:), allocatable :: temp
    if(a<c+n) then 
       if (a>0) then 
          allocate(temp(a))
          temp=s
          deallocate(s)
       end if
       allocate(s(a+store_increase))
       if (a>0) then 
          s(1:a) = temp
          deallocate(temp)
       end if
       a=a+store_increase
    end if
    do i=1,n
!       write(*,'(A,EN26.16E3,I)') "store(v)  ", x(i)%v, c+i
       s(c+i)=x(i)
    end do
    c=c+n
  end subroutine cp_store_p_real_vector_impl

  subroutine cp_store_int_scalar_impl(x,s,c,a)
    ! store x in stack s of allocated size a in current position c
    implicit none
    integer, intent(in) :: x
    integer :: c,a
    integer, dimension(:), allocatable :: s
    ! temp array for potential reallocation
    integer, dimension(:), allocatable :: temp
    if(a<c+1) then 
       if (a>0) then 
          allocate(temp(a))
          temp=s
          deallocate(s)
       end if
       allocate(s(a+store_increase))
       if (a>0) then 
          s(1:a) = temp
          deallocate(temp)
       end if
       a=a+store_increase
    end if
    c=c+1
    s(c)=x
!    write(*,'(A,I5,I5)') "store(s)  ", x,c
  end subroutine cp_store_int_scalar_impl

  subroutine cp_store_int_vector_impl(x,n,s,c,a)
    ! store x of size n in stack s of allocated size a in current position c
    implicit none
    integer, dimension(:), intent(in) :: x
    integer, intent(in) :: n
    integer :: c,a
    integer, dimension(:), allocatable :: s
    ! temp array for potential reallocation
    integer, dimension(:), allocatable :: temp
    if(a<c+n) then 
       if (a>0) then 
          allocate(temp(a))
          temp=s
          deallocate(s)
       end if
       allocate(s(a+store_increase))
       if (a>0) then 
          s(1:a) = temp
          deallocate(temp)
       end if
       a=a+store_increase
    end if
    s(c+1:c+n)=x
    c=c+n
  end subroutine cp_store_int_vector_impl

  subroutine cp_store_string_scalar_impl(x,s,c,a)
    ! store x in stack s of allocated size a in current position c
    implicit none
    character(*), intent(in) :: x
    integer :: c,a
    character(string_length), dimension(:), allocatable :: s
    ! temp array for potential reallocation
    character(string_length), dimension(:), allocatable :: temp
    if(a<c+1) then 
       if (a>0) then 
          allocate(temp(a))
          temp=s
          deallocate(s)
       end if
       allocate(s(a+store_increase))
       if (a>0) then 
          s(1:a) = temp
          deallocate(temp)
       end if
       a=a+store_increase
    end if
    c=c+1
    s(c)=x
  end subroutine cp_store_string_scalar_impl

  subroutine cp_store_bool_scalar_impl(x,s,c,a)
    ! store x in stack s of allocated size a in current position c
    implicit none
    logical, intent(in) :: x
    integer :: c,a
    logical, dimension(:), allocatable :: s
    ! temp array for potential reallocation
    logical, dimension(:), allocatable :: temp
    if(a<c+1) then 
       if (a>0) then 
          allocate(temp(a))
          temp=s
          deallocate(s)
       end if
       allocate(s(a+store_increase))
       if (a>0) then 
          s(1:a) = temp
          deallocate(temp)
       end if
       a=a+store_increase
    end if
    c=c+1
    s(c)=x
  end subroutine cp_store_bool_scalar_impl

end module OAD_cp
