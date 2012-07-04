

!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
        module OAD_active
        use w2f__types
        implicit none
        private :: runTimeErrorStop, shapeChange
        public :: active




        public :: oad_convert
        public :: oad_allocateMatching, oad_allocateShape, oad_shapeTest




        integer, parameter :: shapeChange=0

        !
        ! active needs to be a sequence type
        !  with no initialization
        !
        type active
          sequence
          real(w2f__8) :: v 
        end type
        interface oad_convert
          module procedure convert_d0_a0
          module procedure convert_d1_a1
          module procedure convert_d2_a2
          module procedure convert_d3_a3
          module procedure convert_d4_a4
          module procedure convert_d5_a5
          module procedure convert_d6_a6
          module procedure convert_d7_a7
          module procedure convert_a0_d0
          module procedure convert_a1_d1
          module procedure convert_a2_d2
          module procedure convert_a3_d3
          module procedure convert_a4_d4
          module procedure convert_a5_d5
          module procedure convert_a6_d6
          module procedure convert_a7_d7
          module procedure convert_r0_a0
          module procedure convert_r1_a1
          module procedure convert_r2_a2
          module procedure convert_r3_a3
          module procedure convert_r4_a4
          module procedure convert_r5_a5
          module procedure convert_r6_a6
          module procedure convert_r7_a7
          module procedure convert_a0_r0
          module procedure convert_a1_r1
          module procedure convert_a2_r2
          module procedure convert_a3_r3
          module procedure convert_a4_r4
          module procedure convert_a5_r5
          module procedure convert_a6_r6
          module procedure convert_a7_r7
        end interface

        interface oad_allocateMatching
          module procedure allocateMatching_d1_d1
          module procedure allocateMatching_a1_d1
          module procedure allocateMatching_d1_a1
          module procedure allocateMatching_a1_a1
          module procedure allocateMatching_d2_d2
          module procedure allocateMatching_a2_d2
          module procedure allocateMatching_d2_a2
          module procedure allocateMatching_a2_a2
          module procedure allocateMatching_d3_d3
          module procedure allocateMatching_a3_d3
          module procedure allocateMatching_d3_a3
          module procedure allocateMatching_a3_a3
          module procedure allocateMatching_a4_a4
          module procedure allocateMatching_d4_a4
          module procedure allocateMatching_d5_d5
          module procedure allocateMatching_a5_d5
          module procedure allocateMatching_d5_a5
          module procedure allocateMatching_a5_a5
          module procedure allocateMatching_d6_d6
          module procedure allocateMatching_a6_d6
          module procedure allocateMatching_d6_a6
          module procedure allocateMatching_a6_a6
          module procedure allocateMatching_r1_r1
          module procedure allocateMatching_d1_r1
          module procedure allocateMatching_r1_d1
          module procedure allocateMatching_a1_r1
          module procedure allocateMatching_r1_a1
          module procedure allocateMatching_a2_r2
          module procedure allocateMatching_r2_a2
        end interface 

        interface oad_allocateShape
          module procedure allocateShape_d1
          module procedure allocateShape_d2
        end interface 

        interface oad_shapeTest
          module procedure shapeTest_a1_d1
          module procedure shapeTest_a1_a1
          module procedure shapeTest_d1_a1
          module procedure shapeTest_a2_d2
          module procedure shapeTest_a2_a2
          module procedure shapeTest_d2_a2
          module procedure shapeTest_a3_a3
          module procedure shapeTest_d3_a3
          module procedure shapeTest_a4_a4
          module procedure shapeTest_d4_a4
          module procedure shapeTest_a5_a5
          module procedure shapeTest_d5_a5
          module procedure shapeTest_a5_d5
          module procedure shapeTest_a6_a6
          module procedure shapeTest_d6_a6
          module procedure shapeTest_a6_d6
          module procedure shapeTest_r1_a1
          module procedure shapeTest_r2_a2
          module procedure shapeTest_a1_r1
          module procedure shapeTest_a2_r2
        end interface 

        interface runTimeErrorStop
          module procedure runTimeErrorStopI
        end interface 

        contains
        !
        ! conversions
        !
        subroutine convert_d0_a0(convertTo, convertFrom)
          real(w2f__8), intent(out) :: convertTo
          type(active), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d1_a1(convertTo, convertFrom)
          real(w2f__8), dimension(:), intent(out) :: convertTo
          type(active), dimension(:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d2_a2(convertTo, convertFrom)
          real(w2f__8), dimension(:,:), intent(out) :: convertTo
          type(active), dimension(:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d3_a3(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d4_a4(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d5_a5(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d6_a6(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_d7_a7(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a0_d0(convertTo, convertFrom)
          type(active), intent(inout) :: convertTo
          real(w2f__8), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a1_d1(convertTo, convertFrom)
          type(active), dimension(:), intent(inout) :: convertTo
          real(w2f__8), dimension(:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a2_d2(convertTo, convertFrom)
          type(active), dimension(:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a3_d3(convertTo, convertFrom)
          type(active), dimension(:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a4_d4(convertTo, convertFrom)
          type(active), dimension(:,:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a5_d5(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a6_d6(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a7_d7(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__8), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_r0_a0(convertTo, convertFrom)
          real(w2f__4), intent(out) :: convertTo
          type(active), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r1_a1(convertTo, convertFrom)
          real(w2f__4), dimension(:), intent(out) :: convertTo
          type(active), dimension(:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r2_a2(convertTo, convertFrom)
          real(w2f__4), dimension(:,:), intent(out) :: convertTo
          type(active), dimension(:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r3_a3(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r4_a4(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r5_a5(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r6_a6(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine
        subroutine convert_r7_a7(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:,:,:), intent(out) :: convertTo
          type(active), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a0_r0(convertTo, convertFrom)
          type(active), intent(inout) :: convertTo
          real(w2f__4), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a1_r1(convertTo, convertFrom)
          type(active), dimension(:), intent(inout) :: convertTo
          real(w2f__4), dimension(:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a2_r2(convertTo, convertFrom)
          type(active), dimension(:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a3_r3(convertTo, convertFrom)
          type(active), dimension(:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a4_r4(convertTo, convertFrom)
          type(active), dimension(:,:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a5_r5(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a6_r6(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        subroutine convert_a7_r7(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:,:), intent(inout) :: convertTo
          real(w2f__4), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          convertTo%v=convertFrom
        end subroutine
        !
        ! allocations
        !
        subroutine allocateMatching_d1_d1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_a1_d1(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_d1_a1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:), allocatable :: toBeAllocated
          type(active), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_a1_a1(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:), allocatable :: toBeAllocated
          type(active), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_d2_d2(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_a2_d2(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_d2_a2(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_a2_a2(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_d3_d3(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3)))
        end subroutine
        subroutine allocateMatching_a3_d3(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3)))
        end subroutine
        subroutine allocateMatching_d3_a3(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3)))
        end subroutine
        subroutine allocateMatching_a3_a3(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3)))
        end subroutine
        subroutine allocateMatching_a4_a4(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4)))
        end subroutine
        subroutine allocateMatching_d4_a4(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4)))
        end subroutine
        subroutine allocateMatching_d5_d5(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5)))
        end subroutine
        subroutine allocateMatching_a5_d5(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5)))
        end subroutine
        subroutine allocateMatching_d5_a5(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5)))
        end subroutine
        subroutine allocateMatching_a5_a5(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5)))
        end subroutine
        subroutine allocateMatching_d6_d6(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5),&
               size(allocateMatching,6)))
        end subroutine
        subroutine allocateMatching_a6_d6(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:,:,:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:,:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5),&
               size(allocateMatching,6)))
        end subroutine
        subroutine allocateMatching_d6_a6(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5),&
               size(allocateMatching,6)))
        end subroutine
        subroutine allocateMatching_a6_a6(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:,:,:,:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:,:,:,:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2),&
               size(allocateMatching,3),&
               size(allocateMatching,4),&
               size(allocateMatching,5),&
               size(allocateMatching,6)))
        end subroutine
        subroutine allocateMatching_r1_r1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__4), dimension(:), allocatable :: toBeAllocated
          real(w2f__4), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_d1_r1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__8), dimension(:), allocatable :: toBeAllocated
          real(w2f__4), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_r1_d1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__4), dimension(:), allocatable :: toBeAllocated
          real(w2f__8), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_a1_r1(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:), allocatable :: toBeAllocated
          real(w2f__4), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_r1_a1(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__4), dimension(:), allocatable :: toBeAllocated
          type(active), dimension(:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching)))
        end subroutine
        subroutine allocateMatching_a2_r2(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:,:), allocatable :: toBeAllocated
          real(w2f__4), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        subroutine allocateMatching_r2_a2(toBeAllocated,allocateMatching)
          implicit none
          real(w2f__4), dimension(:,:), allocatable :: toBeAllocated
          type(active), dimension(:,:) :: allocateMatching
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(size(allocateMatching,1), &
               size(allocateMatching,2)))
        end subroutine
        !
        ! allocate shape
        !
        subroutine allocateShape_d1(toBeAllocated,s1)
          implicit none
          real(w2f__8), dimension(:), allocatable :: toBeAllocated
          integer(w2f__i8) :: s1
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(s1))
        end subroutine
        subroutine allocateShape_d2(toBeAllocated,s1,s2)
          implicit none
          real(w2f__8), dimension(:,:), allocatable :: toBeAllocated
          integer(w2f__i8) :: s1,s2
          if (allocated(toBeAllocated)) deallocate(toBeAllocated)
          allocate(toBeAllocated(s1,s2))
        end subroutine
        !
        ! shape tests
        !
        subroutine shapeTest_a1_d1(allocatedVar,origVar)
          implicit none
          type(active), dimension(:), allocatable :: allocatedVar
          real(w2f__8), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a1_a1(allocatedVar,origVar)
          implicit none
          type(active), dimension(:), allocatable :: allocatedVar
          type(active), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d1_a1(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:), allocatable :: allocatedVar
          type(active), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a2_d2(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:), allocatable :: allocatedVar
          real(w2f__8), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a2_a2(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:), allocatable :: allocatedVar
          type(active), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d2_a2(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:), allocatable :: allocatedVar
          type(active), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a3_a3(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d3_a3(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a4_a4(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d4_a4(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a5_a5(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d5_a5(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange) 
        end subroutine
        subroutine shapeTest_a5_d5(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:,:), allocatable :: allocatedVar
          real(w2f__8), dimension(:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange) 
        end subroutine
        subroutine shapeTest_a6_a6(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_d6_a6(allocatedVar,origVar)
          implicit none
          real(w2f__8), dimension(:,:,:,:,:,:), allocatable :: allocatedVar
          type(active), dimension(:,:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange) 
        end subroutine
        subroutine shapeTest_a6_d6(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:,:,:,:,:), allocatable :: allocatedVar
          real(w2f__8), dimension(:,:,:,:,:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange) 
        end subroutine
        subroutine shapeTest_r1_a1(allocatedVar,origVar)
          implicit none
          real(w2f__4), dimension(:), allocatable :: allocatedVar
          type(active), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_r2_a2(allocatedVar,origVar)
          implicit none
          real(w2f__4), dimension(:,:), allocatable :: allocatedVar
          type(active), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a1_r1(allocatedVar,origVar)
          implicit none
          type(active), dimension(:), allocatable :: allocatedVar
          real(w2f__4), dimension(:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine shapeTest_a2_r2(allocatedVar,origVar)
          implicit none
          type(active), dimension(:,:), allocatable :: allocatedVar
          real(w2f__4), dimension(:,:) :: origVar
          if (.not. all(shape(allocatedVar)==shape(origVar))) call runTimeErrorStop(shapeChange)
        end subroutine
        subroutine runTimeErrorStopI(mesgId)
          implicit none
          integer mesgId
          select case (mesgId) 
          case (shapeChange)
             stop "ERROR: OAD run time library: detected shape change"
             end select 
        end subroutine

        end module
