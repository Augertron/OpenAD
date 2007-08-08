subroutine driver_h(h)

  use OpenAD_active
  use OpenAD_rev
  use OpenAD_tape
  use all_globals_mod

  implicit none 

  ! first 2 are independents, 
  ! last dimension is dependents
  double precision, dimension(2*kdim,2*kdim,2*kdim) :: h 

  type(active) , dimension(2*kdim,2*kdim) :: active_g
  integer i,j

  ! reverse for the gradient
  call tape_init()
  do i=1,2*kdim
     do j=1,2*kdim
        ! initialize values (driver_g inits the gradient)
        active_g(:,:)%d=0.0
        active_g(i,j)%d=1.0 
        our_rev_mode%arg_store=.FALSE.
        our_rev_mode%arg_restore=.FALSE.
        our_rev_mode%res_store=.FALSE.
        our_rev_mode%res_restore=.FALSE.
        our_rev_mode%plain=.FALSE.
        our_rev_mode%tape=.TRUE.
        our_rev_mode%adjoint=.FALSE.
        call driver_g(active_g)
        our_rev_mode%arg_store=.FALSE.
        our_rev_mode%arg_restore=.FALSE.
        our_rev_mode%res_store=.FALSE.
        our_rev_mode%res_restore=.FALSE.
        our_rev_mode%plain=.FALSE.
        our_rev_mode%tape=.FALSE.
        our_rev_mode%adjoint=.TRUE.
        call driver_g(active_g)
        do k=1,2*kdim
           h(i,j,k)=xx(k,1)%d
        end do
     end do
  end do

end subroutine driver_h
