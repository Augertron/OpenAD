! ========== begin copyright notice ==============
! This file is part of 
! ---------------
! OpenAD/F
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

        module OpenAD_active
        use w2f__types
        implicit none
        private
        public :: active, saxpy, sax, setderiv, zero_deriv, convert_p2a_scalar, &
&convert_a2p_scalar, convert_p2a_vector, convert_a2p_vector

        
        !
        ! active needs to be a sequence type
        !  with no initialization
        !
        type active
          sequence
          double precision :: v=0.0
          ! initialization does not work for active variables
          ! inside of common block, such as in boxmodel
          ! initialization is required for correct adjoint
          double precision :: d=0.0
          ! double precision :: d
        end type active

        interface saxpy
          module procedure saxpy_a_a
        end interface
        
        interface setderiv
          module procedure setderiv_a_a
        end interface

        interface zero_deriv
          module procedure zero_deriv_a
        end interface
        
        interface sax
          module procedure sax_d_a_a, sax_i_a_a
        end interface

        interface convert_p2a_scalar
          module procedure convert_p2a_scalar_impl
        end interface
        interface convert_a2p_scalar
          module procedure convert_a2p_scalar_impl
        end interface
        interface convert_p2a_vector
          module procedure convert_p2a_vector_impl
        end interface
        interface convert_a2p_vector
          module procedure convert_a2p_vector_impl
        end interface

        contains
        
        !
        ! chain rule saxpy to be used in forward and reverse modes
        !
        
        subroutine saxpy_a_a(a,x,y)
          double precision, intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
        
          y%d=y%d+x%d*a
        end subroutine saxpy_a_a
        
        !
        ! chain rule saxpy to be used in forward and reverse modes
        ! derivative component of y is equal to zero initially
        ! note: y needs to be inout as otherwise value component gets
        ! zeroed out
        !
        
        subroutine sax_d_a_a(a,x,y)
          double precision, intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
        
          y%d=x%d*a
        end subroutine sax_d_a_a

        subroutine sax_i_a_a(a,x,y)
          integer(kind=w2f__i8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
        
          y%d=x%d*a
        end subroutine sax_i_a_a
        
        !
        ! set derivative of y to be equal to derivative of x
        ! note: making y inout allows for already existing active
        ! variables to become the target of a derivative assignment
        !
        
        subroutine setderiv_a_a(y,x)
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
        
          y%d=x%d
        end subroutine setderiv_a_a

        !
        ! set derivative components to 0.0
        !
        subroutine zero_deriv_a(x)
          type(active), intent(inout) :: x

          x%d=0.0d0
        end subroutine zero_deriv_a

        subroutine convert_a2p_scalar_impl(convertTo, convertFrom)
          double precision, intent(inout) :: convertTo
          type(active), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_p2a_scalar_impl(convertTo, convertFrom)
          double precision, intent(in) :: convertFrom
          type(active), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine 

        subroutine convert_a2p_vector_impl(convertTo, convertFrom)
          type(active), dimension(:), intent(in) :: convertFrom
          double precision, dimension(:), intent(inout) :: convertTo
          integer i
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             convertTo(i)=convertFrom(i)%v
          end do
        end subroutine

        subroutine convert_p2a_vector_impl(convertTo, convertFrom)
          double precision, dimension(:), intent(in) :: convertFrom
          type(active), dimension(:), intent(inout) :: convertTo
          integer i
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             convertTo(i)%v=convertFrom(i)
          end do
        end subroutine
        
        end module

