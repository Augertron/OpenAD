        subroutine template()
          use OADtape
          use OADrev

!$TEMPLATE_PRAGMA_DECLARATIONS

          integer iaddr
          external iaddr

         if (our_rev_mode%plain) then
! original function
!$PLACEHOLDER_PRAGMA$ id=1
          end if
          if (our_rev_mode%tape) then
! taping
!$PLACEHOLDER_PRAGMA$ id=2
          end if 
          if (our_rev_mode%adjoint) then
! adjoint
!$PLACEHOLDER_PRAGMA$ id=3
          end if 
        end subroutine template
