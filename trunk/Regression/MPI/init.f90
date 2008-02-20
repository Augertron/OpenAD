subroutine init(x)
 implicit none
 include 'mpif.h'
 ! use mpi
 double precision  x
 integer myid, ierr
 call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
 x=myid*0.5
end
