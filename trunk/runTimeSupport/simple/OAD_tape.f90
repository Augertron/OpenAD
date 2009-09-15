!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
module OAD_tape

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

  double precision   :: double_tape (max_double_tape_size)
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

end module OAD_tape
