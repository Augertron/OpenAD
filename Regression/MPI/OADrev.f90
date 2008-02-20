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
module OADrev

implicit none

private
public :: modeType, our_rev_mode
!, forward_mode, &
!& forward_arg_store_mode, reverse_mode, adjoint_mode, taping_mode, restore_mode

type modeType
  logical :: arg_store=.FALSE.
  logical :: arg_restore=.FALSE.
  logical :: res_store=.FALSE.
  logical :: res_restore=.FALSE.
  logical :: plain=.FALSE.
  logical :: tape=.FALSE.
  logical :: adjoint=.FALSE.
end type modeType

type(modeType), save :: our_rev_mode

! interface forward_mode
!   module procedure forward_mode_i
! end interface
! 
! interface forward_arg_store_mode
!   module procedure forward_arg_store_mode_i
! end interface
! 
! interface taping_mode
!   module procedure taping_mode_i
! end interface
! 
! interface adjoint_mode
!   module procedure adjoint_mode_i
! end interface
! 
! interface reverse_mode
!   module procedure reverse_mode_i
! end interface
! 
! interface restore_mode
!   module procedure restore_mode_i
! end interface
! 
! contains
! 
! subroutine forward_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.FALSE.
!   our_rev_mode%arg_restore=.FALSE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.TRUE.
!   our_rev_mode%tape=.FALSE.
!   our_rev_mode%adjoint=.FALSE.
! end subroutine 
! 
! subroutine forward_arg_store_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.TRUE.
!   our_rev_mode%arg_restore=.FALSE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.TRUE.
!   our_rev_mode%tape=.FALSE.
!   our_rev_mode%adjoint=.FALSE.
! end subroutine 
! 
! subroutine taping_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.FALSE.
!   our_rev_mode%arg_restore=.FALSE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.FALSE.
!   our_rev_mode%tape=.TRUE.
!   our_rev_mode%adjoint=.FALSE.
! end subroutine 
! 
! subroutine adjoint_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.FALSE.
!   our_rev_mode%arg_restore=.FALSE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.FALSE.
!   our_rev_mode%tape=.FALSE.
!   our_rev_mode%adjoint=.TRUE.
! end subroutine 
! 
! subroutine reverse_mode_i()
!   our_orig_mode=our_rev_mode
! 
!   our_rev_mode%arg_store=.FALSE.
!   our_rev_mode%arg_restore=.TRUE.
!   our_rev_mode%res_store=.FALSE.
!   our_rev_mode%res_restore=.FALSE.
!   our_rev_mode%plain=.FALSE.
!   our_rev_mode%tape=.TRUE.
!   our_rev_mode%adjoint=.TRUE.
! end subroutine 
! 
! subroutine restore_mode_i()
!   our_rev_mode=our_orig_mode
! end subroutine 
	
end 
