
        module OADactive
        use w2f__types
        implicit none
        private
        public :: active, saxpy, sax, setderiv, zero_deriv, &
&convert_p2a_scalar, convert_a2p_scalar, &
&convert_p2a_vector, convert_a2p_vector, & 
&convert_p2a_matrix, convert_a2p_matrix, & 
&convert_p2a_three_tensor, convert_a2p_three_tensor, & 
&convert_p2a_four_tensor, convert_a2p_four_tensor, & 
&convert_p2a_five_tensor, convert_a2p_five_tensor 

        
        !
        ! active needs to be a sequence type
        !  with no initialization
        !
        type active
          sequence
          double precision :: v 
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
          module procedure convert_sp2a_vector_impl
        end interface
        interface convert_a2p_vector
          module procedure convert_a2sp_vector_impl
        end interface

        interface convert_p2a_matrix
          module procedure convert_sp2a_matrix_impl
        end interface
        interface convert_a2p_matrix
          module procedure convert_a2sp_matrix_impl
        end interface

        interface convert_p2a_three_tensor
          module procedure convert_sp2a_three_tensor_impl
        end interface
        interface convert_a2p_three_tensor
          module procedure convert_a2sp_three_tensor_impl
        end interface

        interface convert_p2a_four_tensor
          module procedure convert_sp2a_four_tensor_impl
        end interface
        interface convert_a2p_four_tensor
          module procedure convert_a2sp_four_tensor_impl
        end interface

        interface convert_p2a_five_tensor
          module procedure convert_sp2a_five_tensor_impl
        end interface
        interface convert_a2p_five_tensor
          module procedure convert_a2sp_five_tensor_impl
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
          type(active), intent(out) :: x

          x%d=0.0d0
        end subroutine zero_deriv_a

        subroutine convert_a2p_scalar_impl(convertTo, convertFrom)
          double precision, intent(out) :: convertTo
          type(active), intent(in) :: convertFrom
          convertTo=convertFrom%v
        end subroutine

        subroutine convert_p2a_scalar_impl(convertTo, convertFrom)
          double precision, intent(in) :: convertFrom
          type(active), intent(inout) :: convertTo
          convertTo%v=convertFrom
        end subroutine 

        subroutine convert_a2sp_vector_impl(convertTo, convertFrom)
          type(active), dimension(:), intent(in) :: convertFrom
          real, dimension(:), intent(out) :: convertTo
          integer i
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             convertTo(i)=convertFrom(i)%v
          end do
        end subroutine

        subroutine convert_sp2a_vector_impl(convertTo, convertFrom)
          real, dimension(:), intent(in) :: convertFrom
          type(active), dimension(:), intent(inout) :: convertTo
          integer i
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             convertTo(i)%v=convertFrom(i)
          end do
        end subroutine

        subroutine convert_a2sp_matrix_impl(convertTo, convertFrom)
          type(active), dimension(:,:), intent(in) :: convertFrom
          real, dimension(:,:), intent(out) :: convertTo
          integer i,j
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             do j=lbound(convertFrom,2),ubound(convertFrom,2)
                convertTo(i,j)=convertFrom(i,j)%v
             end do
          end do
        end subroutine

        subroutine convert_sp2a_matrix_impl(convertTo, convertFrom)
          real, dimension(:,:), intent(in) :: convertFrom
          type(active), dimension(:,:), intent(inout) :: convertTo
          integer i,j
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             do j=lbound(convertFrom,2),ubound(convertFrom,2)
                convertTo(i,j)%v=convertFrom(i,j)
             end do
          end do
        end subroutine

        subroutine convert_a2sp_three_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:), intent(in) :: convertFrom
          real, dimension(:,:,:), intent(out) :: convertTo
          integer i,j,k
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             do j=lbound(convertFrom,2),ubound(convertFrom,2)
                do k=lbound(convertFrom,3),ubound(convertFrom,3)
                   convertTo(i,j,k)=convertFrom(i,j,k)%v
                end do
             end do
          end do
        end subroutine

        subroutine convert_sp2a_three_tensor_impl(convertTo, convertFrom)
          real, dimension(:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:), intent(inout) :: convertTo
          integer i,j,k
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             do j=lbound(convertFrom,2),ubound(convertFrom,2)
                do k=lbound(convertFrom,3),ubound(convertFrom,3)
                   convertTo(i,j,k)%v=convertFrom(i,j,k)
                end do
             end do
          end do
        end subroutine
        
        subroutine convert_a2sp_four_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:), intent(in) :: convertFrom
          real, dimension(:,:,:,:), intent(out) :: convertTo
          integer i,j,k,l
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             do j=lbound(convertFrom,2),ubound(convertFrom,2)
                do k=lbound(convertFrom,3),ubound(convertFrom,3)
                   do l=lbound(convertFrom,4),ubound(convertFrom,4)
                      convertTo(i,j,k,l)=convertFrom(i,j,k,l)%v
                   end do
                end do
             end do
          end do
        end subroutine

        subroutine convert_sp2a_four_tensor_impl(convertTo, convertFrom)
          real, dimension(:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:), intent(inout) :: convertTo
          integer i,j,k,l
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             do j=lbound(convertFrom,2),ubound(convertFrom,2)
                do k=lbound(convertFrom,3),ubound(convertFrom,3)
                   do l=lbound(convertFrom,4),ubound(convertFrom,4)
                      convertTo(i,j,k,l)%v=convertFrom(i,j,k,l)
                   end do
                end do
             end do
          end do
        end subroutine

        subroutine convert_a2sp_five_tensor_impl(convertTo, convertFrom)
          type(active), dimension(:,:,:,:,:), intent(in) :: convertFrom
          real, dimension(:,:,:,:,:), intent(out) :: convertTo
          integer i,j,k,l,m
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             do j=lbound(convertFrom,2),ubound(convertFrom,2)
                do k=lbound(convertFrom,3),ubound(convertFrom,3)
                   do l=lbound(convertFrom,4),ubound(convertFrom,4)
                      do m=lbound(convertFrom,5),ubound(convertFrom,5)
                         convertTo(i,j,k,l,m)=convertFrom(i,j,k,l,m)%v
                      end do
                   end do
                end do
             end do
          end do
        end subroutine

        subroutine convert_sp2a_five_tensor_impl(convertTo, convertFrom)
          real, dimension(:,:,:,:,:), intent(in) :: convertFrom
          type(active), dimension(:,:,:,:,:), intent(inout) :: convertTo
          integer i,j,k,l,m
          do i=lbound(convertFrom,1),ubound(convertFrom,1)
             do j=lbound(convertFrom,2),ubound(convertFrom,2)
                do k=lbound(convertFrom,3),ubound(convertFrom,3)
                   do l=lbound(convertFrom,4),ubound(convertFrom,4)
                      do m=lbound(convertFrom,5),ubound(convertFrom,5)
                         convertTo(i,j,k,l,m)%v=convertFrom(i,j,k,l,m)
                      end do
                   end do
                end do
             end do
          end do
        end subroutine
        
        end module


