module oad_mpi_wrapper_helper

public :: oad_buffer_helper

double precision oad_buffer_helper(1) 

end 

subroutine  oad_mpi_isend ( & 
     buf, & 
     count, &
     datatype, &
     dest, &
     tag, &
     comm, &
     request, &
     ierror)
!$openad xxx template oad_t_isend.f

  use oad_mpi_wrapper_helper

  double precision buf(*)
  integer count
  integer datatype
  integer dest
  integer tag
  integer comm
  integer request
  integer ierror  

#ifdef OAD_TRANS  
  oad_buffer_helper(1)=buf(1)
#else
  call mpi_isend( &
     buf, & 
     count, &
     datatype, &
     dest, &
     tag, &
     comm, &
     request, &
     ierror)
#endif

end 
