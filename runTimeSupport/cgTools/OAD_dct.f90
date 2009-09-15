!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
module OAD_dct
  implicit none

  private
  public :: dct_node_child, dct_node, dct

  ! need a function pointer here
  ! alternatively, one could write the dct as well as
  ! the reversal scheme in C++

  ! can be made more dynamic
  integer, parameter :: max_nr_local_calls=100

  type dct_node_child
    type(dct_node), pointer :: child
  end type dct_node_child

  type dct_node
    character(80) :: subroutine_name
    integer :: dble_tape_base=1
    integer :: dble_argcp_base=1
    integer :: dble_rescp_base=1
    type(dct_node), pointer :: parent
    type(dct_node_child) :: children(max_nr_local_calls)
  end type dct_node

  ! can be made more dynamic
  integer, parameter :: max_nr_calls=1000
  ! dct(1) is root
  type(dct_node), dimension(max_nr_calls), save :: dct
  ! current dct_node
  integer current

end module OAD_dct
