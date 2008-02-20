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
module OADtape
  implicit none

  private
  public :: double_tape, double_tape_pointer, &
&           integer_tape, integer_tape_pointer, &
&           logical_tape, logical_tape_pointer, &
&           character_tape, character_tape_pointer, &
&           stringlength_tape, stringlength_tape_pointer, &
&           tape_init, tape_dump

  integer, parameter :: max_double_tape_size  =10000000
  integer, parameter :: max_integer_tape_size =10000000
  integer, parameter :: max_logical_tape_size =10000
  integer, parameter :: max_character_tape_size =1000000
  integer, parameter :: max_stringlength_tape_size =1000

  real(8)   :: double_tape (max_double_tape_size)
  integer   :: integer_tape(max_integer_tape_size)
  logical   :: logical_tape(max_logical_tape_size)
  character(max_character_tape_size) :: character_tape
  integer   :: stringlength_tape(max_stringlength_tape_size)

  integer double_tape_pointer,     &
&         integer_tape_pointer,    &
&         logical_tape_pointer,    &
&         character_tape_pointer,  & 
&         stringlength_tape_pointer 

  interface tape_init
    module procedure init
  end interface tape_init

  interface tape_dump
    module procedure dump
  end interface tape_dump

contains

  subroutine init
    double_tape_pointer       = 1
    integer_tape_pointer      = 1
    logical_tape_pointer      = 1
    character_tape_pointer    = 1
    stringlength_tape_pointer = 1
  end subroutine init

  subroutine dump
    integer i
    print*, "double tape"
    do i=1,double_tape_pointer-1
      print*, double_tape(i)
    enddo
    print*, "integer tape"
    do i=1,integer_tape_pointer-1
      print*, integer_tape(i)
    enddo
    print*, "logical tape"
    do i=1,logical_tape_pointer-1
      print*, logical_tape(i)
    enddo
    print*, "character tape"
    print*, character_tape
    print*, "stringlength tape"
    do i=1,stringlength_tape_pointer-1
      print*, stringlength_tape(i)
    enddo
  end subroutine dump

end 
