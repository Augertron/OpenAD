c$openad XXX Template ad_template.f
      subroutine driver_g(g)

      use all_globals_mod
      implicit none 
      external box_ini_params
      external box_ini_fields
      external box_model_body

      double precision, dimension(2*kdim,2*kdim) :: g 
      
      integer i,j

c$openad INDEPENDENT(xx)
c forward approach for each direction
      do i=1,2*kdim
c initialize
         call box_ini_params
         call box_ini_fields
         xx(i,2)=1.0
         call box_model_body
         do j=1,kdim
            g(j,i)=tnew(j,2)
            g(j+kdim,i)=snew(j,2)
         end do
      end do
c$openad DEPENDENT(g)
      end 
