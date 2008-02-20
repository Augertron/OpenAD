program driverAdm
 use OADactive
 use OADrev
 use OADtape

 ! use mpi
 implicit none ! after 'use' before 'include'
 include 'mpif.h'

 type(active) ::  x, f
 integer myid, ierr
 call MPI_INIT(ierr)
 call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
 ! init x
 our_rev_mode%plain=.TRUE.
 call init(x%v)
 ! compute f
 our_rev_mode%plain=.FALSE.
 our_rev_mode%tape=.TRUE.
 call compute(x,f)
 our_rev_mode%tape=.FALSE.
 our_rev_mode%adjoint=.TRUE.
 f%d=1.0
 call compute(x,f)
 ! all nodes print the adjoints.
 print *, myid, ":", x%d
 call MPI_FINALIZE(ierr)
end
