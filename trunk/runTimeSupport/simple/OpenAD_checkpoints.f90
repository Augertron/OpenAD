! ========== begin copyright notice ==============
! This file is part of 
! ---------------
! xaifBooster
! ---------------
! Distributed under the BSD license as follows:
! Copyright (c) 2005, The University of Chicago
! All rights reserved.
!
! Redistribution and use in source and binary forms, 
! with or without modification, are permitted provided that the following conditions are met:
!
!    - Redistributions of source code must retain the above copyright notice, 
!      this list of conditions and the following disclaimer.
!    - Redistributions in binary form must reproduce the above copyright notice, 
!      this list of conditions and the following disclaimer in the documentation 
!      and/or other materials provided with the distribution.
!    - Neither the name of The University of Chicago nor the names of its contributors 
!      may be used to endorse or promote products derived from this software without 
!      specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY 
! EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
! OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
! SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! 
! General Information:
! xaifBooster is intended for the transformation of 
! numerical programs represented as xml files according 
! to the XAIF schema. It is part of the OpenAD framework. 
! The main application is automatic 
! differentiation, i.e. the generation of code for 
! the computation of derivatives. 
! The following people are the principal authors of the 
! current version: 
! 	Uwe Naumann
!	Jean Utke
! Additional contributors are: 
!	Andrew Lyons
!	Peter Fine
!
! For more details about xaifBooster and its use in OpenAD please visit:
!   http://www.mcs.anl.gov/openad
!
! This work is partially supported by:
! 	NSF-ITR grant OCE-0205590
! ========== end copyright notice ==============
module OpenAD_checkpoints

  use active_module

  implicit none

  private

  integer, parameter :: store_increase=20000

  public ::  cp_store_real_scalar, cp_store_real_vector, &
& cp_store_int_scalar, cp_store_int_vector, &
& cp_store_string_scalar, cp_store_bool_scalar , cp_store_p_real_vector

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
!    write(*,'(A,I5)') "store(s)  ", x
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
    character(80), intent(in) :: x
    integer :: c,a
    character(80), dimension(:), allocatable :: s
    ! temp array for potential reallocation
    character(80), dimension(:), allocatable :: temp
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

end module OpenAD_checkpoints
