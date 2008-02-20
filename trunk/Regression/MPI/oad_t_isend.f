        subroutine template()
          use OADtape
          use OADrev

!$TEMPLATE_PRAGMA_DECLARATIONS

          integer iaddr
          external iaddr

         if (our_rev_mode%plain) then
! original function
            call mpi_isend( 
     +      buf,  
     +      count, 
     +      datatype, 
     +      dest, 
     +      tag, 
     +      comm, 
     +      request, 
     +      ierror)
          end if
          if (our_rev_mode%tape) then
! taping
            call mpi_isend( 
     +      buf,  
     +      count, 
     +      datatype, 
     +      dest, 
     +      tag, 
     +      comm, 
     +      request, 
     +      ierror)
          end if 
          if (our_rev_mode%adjoint) then
! adjoint
            call mpi_irecv( 
     +      buf,  
     +      count, 
     +      datatype, 
     +      dest, 
     +      tag, 
     +      comm, 
     +      request, 
     +      ierror)
          end if 
        end subroutine template
