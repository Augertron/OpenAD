      program main

      use all_globals_mod
      integer j

      call box_ini_params
      call box_ini_fields
      call box_model_body

      do j=1,kdim
         write(*,'(A,I1,A,E25.17E3)') "tnew(",j,")=",tnew(j)
         write(*,'(A,I1,A,E25.17E3)') "snew(",j,")=",snew(j)
      end do

      end 
