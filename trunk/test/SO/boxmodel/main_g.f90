program main_g

  use all_globals_mod
  implicit none 
  external box_ini_params
  external box_ini_fields
  external box_model_body
  external driver_g
  integer i,j
  double precision :: h
  double precision tnew_o(kdim), snew_o(kdim)
  double precision jac(2*kdim,2*kdim)

  double precision, dimension(2*kdim,2*kdim) :: g

  h=0.00001

  write(*,*) 'DD: gradient (Jacobian) of function'
  call box_ini_params
  call box_ini_fields
  call box_model_body
  do j=1,kdim
     tnew_o(j)=tnew(j,1)
     snew_o(j)=snew(j,1)
  end do
  do i=1,2*kdim
     call box_ini_params
     call box_ini_fields
     xx(i,1)=xx(i,1)+h
     call box_model_body
     do j=1,kdim
        jac(j,i)=(tnew(j,1)-tnew_o(j))/h
        jac(j+kdim,i)=(snew(j,1)-snew_o(j))/h
     end do
  end do
  do i=1,2*kdim
     do j=1,2*kdim
        write(*,'(A,I2,A,I2,A,E25.17E3)') "J(",i,",",j,")=",jac(i,j)
     end do
  end do

  write(*,*) 'AD: gradient (Jacobian) of function'
  call driver_g(g)
  do i=1,2*kdim
     do j=1,2*kdim
        write(*,'(A,I2,A,I2,A,E25.17E3,A,E25.17E3)') "J(",i,",",j,")=",g(i,j)," diff: ", abs(g(i,j)-jac(i,j))
     end do
  end do

end program main_g
