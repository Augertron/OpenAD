program main_h

  use active_module
  use OpenAD_rev
  use OpenAD_tape

  implicit none 

  double precision , dimension(2) :: x0
  double precision , dimension(2,2) :: Hess
  type(active) , dimension(2,2) :: x,xpp,xmm,xpm,xmp
  type(active) , dimension(2) :: y,ypp,ymm,ypm,ymp
  real h
  integer n,m
  integer i,j,di,dj

  n=2
  m=1
  h=0.001

  do i=1,n
    x0(i)=i*1.0
    x(i,1)%v=x0(i)
  end do

  our_rev_mode%arg_store=.FALSE.
  our_rev_mode%arg_restore=.FALSE.
  our_rev_mode%res_store=.FALSE.
  our_rev_mode%res_restore=.FALSE.
  our_rev_mode%plain=.TRUE.
  our_rev_mode%tape=.FALSE.
  our_rev_mode%adjoint=.FALSE.
  call head(x,y)
  write(*,*) 'second order DD: of function'
  do i=1,n   
    do j=1,n   
       ! reinit the variables
       do di=1,n   
         xpp(di,1)%v=x0(di)
         xmm(di,1)%v=x0(di)
         xmp(di,1)%v=x0(di)
         xpm(di,1)%v=x0(di)
       end do
       if (i==j) then 
         xpp(i,1)%v=x0(i)+h
         xmm(i,1)%v=x0(i)-h
         call head(xpp,ypp)
         call head(xmm,ymm)
         write(*,'(A,I3,A,I3,A,E25.17E3)') "H(",i,",",j,")=",(ypp(1)%v+ymm(1)%v-2*y(1)%v)/(h**2)
       else
         xpp(i,1)%v=x0(i)+h
         xpp(j,1)%v=x0(j)+h
         xmm(i,1)%v=x0(i)-h
         xmm(j,1)%v=x0(j)-h
         xmp(i,1)%v=x0(i)-h
         xmp(j,1)%v=x0(j)+h
         xpm(i,1)%v=x0(i)+h
         xpm(j,1)%v=x0(j)-h
         call head(xpp,ypp)
         call head(xmm,ymm)
         call head(xmp,ymp)
         call head(xpm,ypm)
         write(*,'(A,I3,A,I3,A,E25.17E3)') "H(",i,",",j,")=",(ypp(1)%v-ypm(1)%v-ymp(1)%v+ymm(1)%v)/(4*(h**2))
       end if
     end do
  end do

  write(*,*) 'AD: of gradient'
  call driver_h(x,Hess)
  do i=1,n   
     do j=1,n
        write(*,'(A,I3,A,I3,A,E25.17E3)') "H(",i,",",j,")=",Hess(i,j)
     end do
  end do

end program main_h
