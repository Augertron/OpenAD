!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
        subroutine saxpy_s0_s0_s0(a,x,y)
          use OAD_active
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          y%d=y%d + x%d*a
        end subroutine
        subroutine saxpy_s0_s1_s1(a,x,y)
          use OAD_active
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x(:)
          type(active), intent(inout) :: y(:)
          do oad_ctmp_i1=lbound(y,1),ubound(y,1)
            y(oad_ctmp_i1)%d=y(oad_ctmp_i1)%d + x(oad_ctmp_i1)%d*a
          end do
        end subroutine
        subroutine saxpy_s1_s0_s1(a,x,y)
          use OAD_active
          real(w2f__8), intent(in) :: a(:)
          type(active), intent(in) :: x
          type(active), intent(inout) :: y(:)
          do oad_ctmp_i1=lbound(y,1),ubound(y,1)
            y(oad_ctmp_i1)%d=y(oad_ctmp_i1)%d + x%d*a(oad_ctmp_i1)
          end do
        end subroutine
        subroutine sax_s0_s0_s0(a,x,y)
          use OAD_active
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x
          type(active), intent(inout) :: y
          y%d=x%d*a
        end subroutine
        subroutine sax_s0_s1_s1(a,x,y)
          use OAD_active
          real(w2f__8), intent(in) :: a
          type(active), intent(in) :: x(:)
          type(active), intent(inout) :: y(:)
          do oad_ctmp_i1=lbound(y,1),ubound(y,1)
            y(oad_ctmp_i1)%d=x(oad_ctmp_i1)%d*a
          end do
        end subroutine
        subroutine sax_s1_s0_s1(a,x,y)
          use OAD_active
          real(w2f__8), intent(in) :: a(:)
          type(active), intent(in) :: x
          type(active), intent(inout) :: y(:)
          do oad_ctmp_i1=lbound(y,1),ubound(y,1)
            y(oad_ctmp_i1)%d=x%d*a(oad_ctmp_i1)
          end do
        end subroutine
        subroutine setderiv_s0_s0(y,x)
          use OAD_active
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          y%d = x%d
        end subroutine
        subroutine setderiv_s1_s1(y,x)
          use OAD_active
          type(active), intent(inout) :: y(:)
          type(active), intent(in) :: x(:)
          do oad_ctmp_i1=lbound(y,1),ubound(y,1)
            y(oad_ctmp_i1)%d = x(oad_ctmp_i1)%d
          end do
        end subroutine
        subroutine set_neg_deriv_s0_s0(y,x)
          use OAD_active
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          y%d = -x%d
        end subroutine
        subroutine inc_deriv_s0_s0(y,x)
          use OAD_active
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
          y%d=y%d + x%d
        end subroutine
        subroutine dec_deriv_s0_s0(y,x)
          use OAD_active
          type(active), intent(inout) :: y
          type(active), intent(in) :: x
            y%d=y%d - x%d
        end subroutine
        subroutine zero_deriv_s0(x)
          use OAD_active
          type(active), intent(inout) :: x
          x%d=0.0d0
        end subroutine
