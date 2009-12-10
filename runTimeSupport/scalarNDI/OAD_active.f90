!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
        module OAD_active

        use w2f__types
        implicit none
        private
        public :: active, saxpy, sax, zero_deriv, &
setderiv, set_neg_deriv, inc_deriv, dec_deriv, &
convert_p2a_scalar, convert_a2p_scalar, &
convert_p2a_vector, convert_a2p_vector, &
convert_p2a_matrix, convert_a2p_matrix, &
convert_p2a_three_tensor, convert_a2p_three_tensor, &
convert_p2a_four_tensor, convert_a2p_four_tensor, &
convert_p2a_five_tensor, convert_a2p_five_tensor, & 
convert_p2a_six_tensor, convert_a2p_six_tensor, &
convert_p2a_seven_tensor, convert_a2p_seven_tensor, &
oad_allocateMatching

        integer :: count_mult = 0
        integer :: count_add = 0

        !
        ! active needs to be a sequence type
        !  with no initialization
        !
        type active
          sequence
          real(w2f__8) :: v 
          real(w2f__8) :: d
        end type active

        interface saxpy
          module procedure saxpy_d_a_a, saxpy_i8_a_a, saxpy_i4_a_a
        end interface
        
        interface setderiv
          module procedure setderiv_a_a
          module procedure setderiv_av_av
        end interface

        interface set_neg_deriv
          module procedure set_neg_deriv_a_a
          module procedure set_neg_deriv_av_av
        end interface set_neg_deriv

        interface inc_deriv
          module procedure inc_deriv_a_a
          module procedure inc_deriv_av_av
        end interface inc_deriv

        interface dec_deriv
          module procedure dec_deriv_a_a
          module procedure dec_deriv_av_av
        end interface dec_deriv

        interface zero_deriv
          module procedure zero_deriv_a
          module procedure zero_deriv_av
          module procedure zero_deriv_am
          module procedure zero_deriv_am3
          module procedure zero_deriv_am4
        end interface
        
        interface sax
          module procedure sax_d_a_a, sax_i8_a_a, sax_i4_a_a
        end interface

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
        ! chain rule saxpy to be used in forward and reverse modes
        !
        
        subroutine saxpy_d_a_a(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          y%d=y%d+x%d*a
        end subroutine saxpy_d_a_a
        
        subroutine saxpy_i8_a_a(a,x,y)
          integer(kind=w2f__i8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          y%d=y%d+x%d*a
        end subroutine saxpy_i8_a_a
        
        subroutine saxpy_i4_a_a(a,x,y)
          integer(kind=w2f__i4), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          y%d=y%d+x%d*a
        end subroutine saxpy_i4_a_a
        
        !
        ! chain rule saxpy to be used in forward and reverse modes
        ! derivative component of y is equal to zero initially
        ! note: y needs to be inout as otherwise value component gets
        ! zeroed out
        !
        
        subroutine sax_d_a_a(a,x,y)
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          y%d=x%d*a
        end subroutine sax_d_a_a

        subroutine sax_i8_a_a(a,x,y)
          integer(kind=w2f__i8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          y%d=x%d*a
        end subroutine sax_i8_a_a
        
        subroutine sax_i4_a_a(a,x,y)
          integer(kind=w2f__i4), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          y%d=x%d*a
        end subroutine sax_i4_a_a
        
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

        subroutine setderiv_av_av(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in), dimension(:) :: x
          y%d=x%d
        end subroutine setderiv_av_av

        !
        ! set the derivative of y to be the negated derivative of x
        ! note: making y inout allows for already existing active
        ! variables to become the target of a derivative assignment
        !
        
        subroutine set_neg_deriv_a_a(y,x)
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          y%d = -x%d
        end subroutine set_neg_deriv_a_a

        subroutine set_neg_deriv_av_av(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in), dimension(:) :: x
          y%d = -x%d
        end subroutine set_neg_deriv_av_av

        !
        ! increment the derivative of y by the derivative of x
        ! note: making y inout allows for already existing active
        ! variables to become the target of a derivative assignment
        !
        
        subroutine inc_deriv_a_a(y,x)
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          y%d = y%d + x%d
        end subroutine inc_deriv_a_a

        subroutine inc_deriv_av_av(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in), dimension(:) :: x
          y%d = y%d + x%d
        end subroutine inc_deriv_av_av

        !
        ! decrement the derivative of y by the derivative of x
        ! note: making y inout allows for already existing active
        ! variables to become the target of a derivative assignment
        !
        
        subroutine dec_deriv_a_a(y,x)
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          y%d = y%d - x%d
        end subroutine dec_deriv_a_a

        subroutine dec_deriv_av_av(y,x)
          type(active), intent(inout), dimension(:) :: y
          type(active), intent(in), dimension(:) :: x
          y%d = y%d - x%d
        end subroutine dec_deriv_av_av

        !
        ! set derivative components to 0.0
        !
        subroutine zero_deriv_a(x)
          type(active), intent(inout) :: x
          x%d=0.0d0
        end subroutine zero_deriv_a

        subroutine zero_deriv_av(x)
          type(active), dimension(:), intent(inout) :: x
          x%d=0.0d0
        end subroutine zero_deriv_av

        subroutine zero_deriv_am(x)
          type(active), dimension(:,:), intent(inout) :: x
          x%d = 0.0d0
        end subroutine zero_deriv_am

        subroutine zero_deriv_am3(x)
          type(active), dimension(:,:,:), intent(inout) :: x
          x%d = 0.0d0
        end subroutine zero_deriv_am3

        subroutine zero_deriv_am4(x)
          type(active), dimension(:,:,:,:), intent(inout) :: x
          x%d = 0.0d0
        end subroutine zero_deriv_am4

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

