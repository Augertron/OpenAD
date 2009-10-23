!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
        module OAD_active

        use w2f__types
        implicit none
        private
        public :: active,  &
convert_p2a_scalar, convert_a2p_scalar, &
convert_p2a_vector, convert_a2p_vector, &
convert_p2a_matrix, convert_a2p_matrix, &
convert_p2a_three_tensor, convert_a2p_three_tensor, &
convert_p2a_four_tensor, convert_a2p_four_tensor, &
convert_p2a_five_tensor, convert_a2p_five_tensor, & 
convert_p2a_six_tensor, convert_a2p_six_tensor, &
convert_p2a_seven_tensor, convert_a2p_seven_tensor, &
oad_allocateMatching

        !
        ! active needs to be a sequence type
        !  with no initialization
        !
        type active
          sequence
          real(w2f__8) :: v 
        end type active

        interface convert_p2a_scalar
          module procedure convert_sp2a_scalar_impl
          module procedure convert_p2a_scalar_impl
        end interface
        interface convert_a2p_scalar
          module procedure convert_a2sp_scalar_impl
          module procedure convert_a2p_scalar_impl
        end interface

        interface convert_p2a_vector
          module procedure convert_sp2a_vector_impl
          module procedure convert_p2a_vector_impl
        end interface
        interface convert_a2p_vector
          module procedure convert_a2sp_vector_impl
          module procedure convert_a2p_vector_impl
        end interface

        interface convert_p2a_matrix
          module procedure convert_sp2a_matrix_impl
          module procedure convert_p2a_matrix_impl
        end interface
        interface convert_a2p_matrix
          module procedure convert_a2sp_matrix_impl
          module procedure convert_a2p_matrix_impl
        end interface

        interface convert_p2a_three_tensor
          module procedure convert_sp2a_three_tensor_impl
          module procedure convert_p2a_three_tensor_impl
        end interface
        interface convert_a2p_three_tensor
          module procedure convert_a2sp_three_tensor_impl
          module procedure convert_a2p_three_tensor_impl
        end interface

        interface convert_p2a_four_tensor
          module procedure convert_sp2a_four_tensor_impl
          module procedure convert_p2a_four_tensor_impl
        end interface
        interface convert_a2p_four_tensor
          module procedure convert_a2sp_four_tensor_impl
          module procedure convert_a2p_four_tensor_impl
        end interface

        interface convert_p2a_five_tensor
          module procedure convert_sp2a_five_tensor_impl
          module procedure convert_p2a_five_tensor_impl
        end interface
        interface convert_a2p_five_tensor
          module procedure convert_a2sp_five_tensor_impl
          module procedure convert_a2p_five_tensor_impl
        end interface

        interface convert_p2a_six_tensor
          module procedure convert_sp2a_six_tensor_impl
          module procedure convert_p2a_six_tensor_impl
        end interface
        interface convert_a2p_six_tensor
          module procedure convert_a2sp_six_tensor_impl
          module procedure convert_a2p_six_tensor_impl
        end interface

        interface convert_p2a_seven_tensor
          module procedure convert_sp2a_seven_tensor_impl
          module procedure convert_p2a_seven_tensor_impl
        end interface
        interface convert_a2p_seven_tensor
          module procedure convert_a2sp_seven_tensor_impl
          module procedure convert_a2p_seven_tensor_impl
        end interface

        interface oad_allocateMatching
          module procedure oad_allocateMatchingV
        end interface 

        contains

        !
        ! active/passive conversions
        !
        subroutine convert_a2sp_scalar_impl(convertTo, convertFrom)
          real(w2f__4), intent(out) :: convertTo
          type(active), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a2p_scalar_impl(convertTo, convertFrom)
          real(w2f__8), intent(out) :: convertTo
          type(active), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_sp2a_scalar_impl(convertTo, convertFrom)
          real(w2f__4), intent(in) :: convertFrom
          type(active), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine 

        subroutine convert_p2a_scalar_impl(convertTo, convertFrom)
          real(w2f__8), intent(in) :: convertFrom
          type(active), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine 

        subroutine convert_a2sp_vector_impl(convertTo, convertFrom)
          type(active), dimension(:), intent(in) :: convertFrom
          real(w2f__4), dimension(:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a2p_vector_impl(convertTo, convertFrom)
          type(active), dimension(:), intent(in) :: convertFrom
          real(w2f__8), dimension(:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_sp2a_vector_impl(convertTo, convertFrom)
          real(w2f__4), dimension(:), intent(in) :: convertFrom
          type(active), dimension(:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_p2a_vector_impl(convertTo, convertFrom)
          real(w2f__8), dimension(:), intent(in) :: convertFrom
          type(active), dimension(:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_a2sp_matrix_impl(convertTo, convertFrom)
          type(active), dimension(:,:), intent(in) :: convertFrom
          real(w2f__4), dimension(:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_sp2a_matrix_impl(convertTo, convertFrom)
          real(w2f__4), dimension(:,:), intent(in) :: convertFrom
          type(active), dimension(:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_a2p_matrix_impl(convertTo, convertFrom)
          type(active), dimension(:,:), intent(in) :: convertFrom
          real(w2f__8), dimension(:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_p2a_matrix_impl(convertTo, convertFrom)
          real(w2f__8), dimension(:,:), intent(in) :: convertFrom
          type(active), dimension(:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_a2sp_three_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:), intent(in) :: convertFrom
          real(w2f__4), dimension(:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a2p_three_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:), intent(in) :: convertFrom
          real(w2f__8), dimension(:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_sp2a_three_tensor_impl(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine
        
        subroutine convert_p2a_three_tensor_impl(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine
        
        subroutine convert_a2sp_four_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:), intent(in) :: convertFrom
          real(w2f__4), dimension(:,:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a2p_four_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:), intent(in) :: convertFrom
          real(w2f__8), dimension(:,:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_sp2a_four_tensor_impl(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_p2a_four_tensor_impl(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_a2sp_five_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:), intent(in) :: convertFrom
          real(w2f__4), dimension(:,:,:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a2p_five_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:), intent(in) :: convertFrom
          real(w2f__8), dimension(:,:,:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_sp2a_five_tensor_impl(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_p2a_five_tensor_impl(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine
        
        subroutine convert_a2sp_six_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          real(w2f__4), dimension(:,:,:,:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a2p_six_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          real(w2f__8), dimension(:,:,:,:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_sp2a_six_tensor_impl(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_p2a_six_tensor_impl(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine
        
        subroutine convert_a2sp_seven_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          real(w2f__4), dimension(:,:,:,:,:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_a2p_seven_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          real(w2f__8), dimension(:,:,:,:,:,:,:), intent(out) :: convertTo
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_sp2a_seven_tensor_impl(convertTo, convertFrom)
          real(w2f__4), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:,:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine convert_p2a_seven_tensor_impl(convertTo, convertFrom)
          real(w2f__8), dimension(:,:,:,:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:,:,:,:), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine

        subroutine oad_allocateMatchingV(toBeAllocated,allocateMatching)
          implicit none
          type(active), dimension(:), allocatable :: toBeAllocated
          type(active), dimension(:) :: allocateMatching
          allocate(toBeAllocated(size(allocateMatching)));
        end subroutine

        end module OAD_active

