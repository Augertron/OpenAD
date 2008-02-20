program driver

 ! use mpi
 implicit none
 include 'mpif.h'

 double precision  x, f
 integer myid, ierr
 call MPI_INIT(ierr)
 call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
 ! init x
 call init(x)
 ! compute f
 call compute(x,f)
 ! node 0 prints the answer.
 if (myid .eq. 0) then
    print *, f
 endif
 call MPI_FINALIZE(ierr)

end
