c$openad XXX Template ad_template.f
      subroutine oadsaxpy(a,x,y)
      double precision, intent(in) :: a
      double precision, dimension(2), intent(in) :: x
      double precision, dimension(2), intent(inout) :: y
      y(2)=y(2)+x(2)*a
      end subroutine 
      
c$openad XXX Template ad_template.f
      subroutine oadsax(a,x,y)
      double precision, intent(in) :: a
      double precision, dimension(2), intent(in) :: x
      double precision, dimension(2), intent(inout) :: y
      y(2)=x(2)*a
      end subroutine

c$openad XXX Template ad_template.f
      subroutine oadsetderiv(y,x)
      double precision, dimension(2), intent(inout) :: y
      double precision, dimension(2), intent(in) :: x
      y(2)=x(2)
      end subroutine 
    
c$openad XXX Template ad_template.f
      subroutine oadzero_deriv(x)
      double precision, dimension(2), intent(inout) :: x
      x(2)=0.0d0
      end subroutine

c$openad XXX Template ad_template.f
      subroutine convert_a2p_scalar_impl(convertTo, convertFrom)
      double precision, intent(inout) :: convertTo
      double precision, dimension(2), intent(in) :: convertFrom
      convertTo=convertFrom(1)
      end subroutine

c$openad XXX Template ad_template.f
      subroutine convert_p2a_scalar_impl(convertTo, convertFrom)
      double precision, intent(in) :: convertFrom
      double precision, dimension(2), intent(inout) :: convertTo
      convertTo(1)=convertFrom
      end subroutine 

c$openad XXX Template ad_template.f
      subroutine convert_a2p_vector_impl(convertTo, convertFrom)
      double precision, dimension(:,:), intent(in) :: convertFrom
      double precision, dimension(:), intent(inout) :: convertTo
      integer i
      do i=lbound(convertFrom,1),ubound(convertFrom,1)
         convertTo(i)=convertFrom(i,1)
      end do
      end subroutine

c$openad XXX Template ad_template.f
      subroutine convert_p2a_vector_impl(convertTo, convertFrom)
      double precision, dimension(:), intent(in) :: convertFrom
      double precision, dimension(:,:), intent(inout) :: convertTo
      integer i
      do i=lbound(convertFrom,1),ubound(convertFrom,1)
         convertTo(i,1)=convertFrom(i)
      end do
      end subroutine
