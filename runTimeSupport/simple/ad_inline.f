!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################

C Checkpoint counter functions---------------------

      subroutine countcheckpoint()
C $OpenAD$ INLINE DECLS
        implicit none
C $OpenAD$ END DECLS
        if(theSwitch.eq.0) then
          print *,"Checkpoint storage"
          print *, "Floating point: ", theArgFStackoffset
          print *, "Integer: ", theArgIStackoffset
          print *, "Boolean: ", theArgBStackoffset
          print *, "String: ", theArgSStackoffset
          theSwitch = 1
        end if
      end subroutine
        

C Counter functions -------------------------------

       subroutine countmult(x)
C $OpenAD$ INLINE DECLS
         implicit none
         integer x
C $OpenAD$ END DECLS
         count_mult = count_mult + x
       end subroutine

       subroutine countadd(x)
C $OpenAD$ INLINE DECLS
         implicit none
         integer x
C $OpenAD$ END DECLS
         count_add = count_add + x
       end subroutine




C taping --------------------------------------------


      subroutine push_s0(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      double precision :: x
C $OpenAD$ END DECLS
        double_tape(double_tape_pointer)=x
        double_tape_pointer=double_tape_pointer+1
      end subroutine 

      subroutine pop_s0(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      double precision :: x
C $OpenAD$ END DECLS
        double_tape_pointer=double_tape_pointer-1
        x=double_tape(double_tape_pointer)
      end subroutine

      subroutine push_s1(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      double precision :: x(:)
C $OpenAD$ END DECLS
        double_tape(double_tape_pointer:double_tape_pointer+size(x)-1)=x(:)
        double_tape_pointer=double_tape_pointer+size(x)
      end subroutine 

      subroutine pop_s1(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      double precision :: x(:)
C $OpenAD$ END DECLS
        double_tape_pointer=double_tape_pointer-size(x)
        x(:)=double_tape(double_tape_pointer:double_tape_pointer+size(x)-1)
      end subroutine

      subroutine push_s2(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      double precision :: x(:,:)
C $OpenAD$ END DECLS
        double_tape(double_tape_pointer:)=reshape(x,(/size(x,1)*size(x,2)/))
        double_tape_pointer=double_tape_pointer+(size(x,1)*size(x,2))
      end subroutine 

      subroutine pop_s2(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      double precision :: x(:,:)
C $OpenAD$ END DECLS
        double_tape_pointer=double_tape_pointer-(size(x,1)*size(x,2))
        x(:,:)=reshape(double_tape(double_tape_pointer:),shape(x))
      end subroutine

      subroutine apush(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      type(active) :: x
C $OpenAD$ END DECLS
        double_tape(double_tape_pointer)=x%v
        double_tape_pointer=double_tape_pointer+1
      end subroutine 

      subroutine apop(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      type(active) :: x
C $OpenAD$ END DECLS
        double_tape_pointer=double_tape_pointer-1
        x%v=double_tape(double_tape_pointer)
      end subroutine

      subroutine push_i_s0(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      integer :: x
C $OpenAD$ END DECLS
        integer_tape(integer_tape_pointer)=x
        integer_tape_pointer=integer_tape_pointer+1
      end subroutine 

      subroutine pop_i_s0(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      integer :: x
C $OpenAD$ END DECLS
        integer_tape_pointer=integer_tape_pointer-1
        x=integer_tape(integer_tape_pointer)
      end subroutine

      subroutine push_i_s1(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      integer :: x(:)
C $OpenAD$ END DECLS
        integer_tape(integer_tape_pointer:integer_tape_pointer+size(x)-1)=x(:)
        integer_tape_pointer=integer_tape_pointer+size(x)
      end subroutine 

      subroutine pop_i_s1(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      integer :: x(:)
C $OpenAD$ END DECLS
        integer_tape_pointer=integer_tape_pointer-size(x)
        x(:)=integer_tape(integer_tape_pointer:integer_tape_pointer+size(x)-1)
      end subroutine

      subroutine push_i_s2(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      integer :: x(:,:)
C $OpenAD$ END DECLS
        integer_tape(integer_tape_pointer:)=reshape(x,(/size(x,1)*size(x,2)/))
        integer_tape_pointer=integer_tape_pointer+(size(x,1)*size(x,2))
      end subroutine 

      subroutine pop_i_s2(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      integer :: x(:,:)
C $OpenAD$ END DECLS
        integer_tape_pointer=integer_tape_pointer-(size(x,1)*size(x,2))
        x(:,:)=reshape(integer_tape(integer_tape_pointer:),shape(x))
      end subroutine

      subroutine push_b(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      logical :: x
C $OpenAD$ END DECLS
        logical_tape(logical_tape_pointer)=x
        logical_tape_pointer=logical_tape_pointer+1
      end subroutine 

      subroutine pop_b(x)
C $OpenAD$ INLINE DECLS
      use OpenAD_tape
      implicit none
      logical :: x
C $OpenAD$ END DECLS
        logical_tape_pointer=logical_tape_pointer-1
        x=logical_tape(logical_tape_pointer)
      end subroutine

      subroutine push_s(s)
C $OpenAD$ INLINE DECLS
        use OpenAD_tape
        implicit none
        character(*) :: s
C $OpenAD$ END DECLS
        stringlength_tape(stringlength_tape_pointer)=len(s)
        stringlength_tape_pointer=stringlength_tape_pointer+1
        character_tape(character_tape_pointer:
     +character_tape_pointer+len(s))=s(1:len(s))
        character_tape_pointer=character_tape_pointer+len(s)
      end subroutine 


      subroutine pop_s(s)
C $OpenAD$ INLINE DECLS
        use OpenAD_tape
        implicit none
        character(*) :: s
C $OpenAD$ END DECLS
        stringlength_tape_pointer=stringlength_tape_pointer-1
        character_tape_pointer=character_tape_pointer-
     +stringlength_tape(stringlength_tape_pointer)
        s(1:len(s))=character_tape(character_tape_pointer:
     +character_tape_pointer+
     +stringlength_tape(stringlength_tape_pointer))
      end subroutine

C ----------------------- Propagation -----------------------

      subroutine saxpy(a,x,y)
C $OpenAD$ INLINE DECLS
      double precision, intent(in) :: a
      type(active), intent(in) :: x
      type(active), intent(inout) :: y
C $OpenAD$ END DECLS
      y%d=y%d+x%d*(a)
      end subroutine

      subroutine zeroderiv(x)
C $OpenAD$ INLINE DECLS
      type(active), intent(out) :: x
C $OpenAD$ END DECLS
      x%d=0.0d0
      end subroutine

      subroutine setderiv(y,x)
C $OpenAD$ INLINE DECLS
      type(active), intent(out) :: x
      type(active), intent(in) :: y
C $OpenAD$ END DECLS
      x%d=y%d
      end subroutine

      subroutine incderiv(y,x)
C $OpenAD$ INLINE DECLS
      type(active), intent(out) :: x
      type(active), intent(in) :: y
C $OpenAD$ END DECLS
      x%d=x%d+y%d
      end subroutine

      subroutine decderiv(y,x)
C $OpenAD$ INLINE DECLS
      type(active), intent(out) :: x
      type(active), intent(in) :: y
C $OpenAD$ END DECLS
      x%d = x%d - y%d
      end subroutine decderiv

C Checkpointing stuff ---------------------------------------

C active reals ----------------------------------------------
      subroutine cp_arg_store_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision :: x
C $OpenAD$ END DECLS
        call cp_store_real_scalar(x%v,theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
      end subroutine 


      subroutine cp_arg_restore_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision :: x
C $OpenAD$ END DECLS
        x%v=theArgFStack(theArgFStackoffset)
C        write(*,'(A,EN26.16E3)') "restore(s)  ", x%v
        theArgFStackoffset=theArgFStackoffset-1
      end subroutine 


      subroutine cp_res_store_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision :: x
C $OpenAD$ END DECLS
        call cp_store_real_scalar(x%v,theResFStack,theResFStackoffset,
     +theResFStackSize)
      end subroutine 


      subroutine cp_res_restore_real_scalar_a(x)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision :: x
C $OpenAD$ END DECLS
C        print*, "restore idx, value, x ", theResFStackoffset, x%v
        x%v=theResFStack(theResFStackoffset)
        theResFStackoffset=theResFStackoffset+1
      end subroutine 


      subroutine cp_arg_store_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        call cp_store_real_vector(x,size(x),
     +theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
      end subroutine 


      subroutine cp_arg_restore_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
           x(cp_loop_variable_1)%v=theArgFStack(theArgFStackoffset)
           theArgFStackoffset=theArgFStackoffset-1
C        write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +x(cp_loop_variable_1)%v
        end do
      end subroutine 


      subroutine cp_res_store_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        call cp_store_real_vector(x,size(x),
     +theResFStack,theResFStackoffset,
     +theResFStackSize)
      end subroutine 


      subroutine cp_res_restore_real_vector_a(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1),1
           x(cp_loop_variable_1)%v=theResFStack(theResFStackoffset)
           theResFStackoffset=theResFStackoffset+1
        end do
      end subroutine 


      subroutine cp_arg_store_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1)
        call cp_store_real_vector(x(cp_loop_variable_1,:),
     +size(x(cp_loop_variable_1,:)),theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
        end do
      end subroutine 


      subroutine cp_arg_restore_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
           do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
            x(cp_loop_variable_1,cp_loop_variable_2)%v=
     +theArgFStack(theArgFStackoffset)
            theArgFStackoffset=theArgFStackoffset-1
           end do
        end do
      end subroutine 


      subroutine cp_res_store_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1)
        call cp_store_real_vector(x(cp_loop_variable_1,:),
     +size(x(cp_loop_variable_1,:)),theResFStack,theResFStackoffset,
     +theResFStackSize)
        end do
      end subroutine 


      subroutine cp_res_restore_real_matrix_a(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,2),1
           do cp_loop_variable_2=lbound(x,2),ubound(x,2),1
            x(cp_loop_variable_1,cp_loop_variable_2)%v=
     +theResFStack(theResFStackoffset)
            theResFStackoffset=theResFStackoffset+1
           end do
        end do
      end subroutine 


      subroutine cp_arg_store_real_four_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1)
           do cp_loop_variable_2=lbound(x,2),ubound(x,2)
            do cp_loop_variable_3=lbound(x,3),ubound(x,3)
        call cp_store_real_vector(x(cp_loop_variable_1,
     +cp_loop_variable_2, cp_loop_variable_3,:),
     +size(x(cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable_3,:)),
     +theArgFStack,
     +theArgFStackoffset,
     +theArgFStackSize)
             end do
           end do
        end do
      end subroutine 


      subroutine cp_arg_restore_real_four_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
           do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
            do cp_loop_variable_3=ubound(x,3),lbound(x,3),-1
               do cp_loop_variable_4=ubound(x,4),lbound(x,4),-1
                  x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4)%v=
     +theArgFStack(theArgFStackoffset)
                  theArgFStackoffset=theArgFStackoffset-1
               end do
            end do
           end do
        end do
        end subroutine 


      subroutine cp_res_store_real_four_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1)
           do cp_loop_variable_2=lbound(x,2),ubound(x,2)
            do cp_loop_variable_3=lbound(x,3),ubound(x,3)
        call cp_store_real_vector(x(cp_loop_variable_1,
     +cp_loop_variable_2, cp_loop_variable_3,:),
     +size(x(cp_loop_variable_1,cp_loop_variable_2, cp_loop_variable_3,:)),
     +theResFStack,
     +theResFStackoffset,
     +theResFStackSize)
            end do
           end do
        end do
      end subroutine 


      subroutine cp_res_restore_real_four_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1)
           do cp_loop_variable_2=lbound(x,2),ubound(x,2)
            do cp_loop_variable_3=lbound(x,3),ubound(x,3)
               do cp_loop_variable_4=lbound(x,4),ubound(x,4)
                  x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4)%v=
     +theResFStack(theResFStackoffset)
                  theResFStackoffset=theResFStackoffset+1
               end do
            end do
           end do
        end do
      end subroutine 

      subroutine cp_arg_store_real_five_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4,
     +cp_loop_variable_5)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1)
           do cp_loop_variable_2=lbound(x,2),ubound(x,2)
            do cp_loop_variable_3=lbound(x,3),ubound(x,3)
               do cp_loop_variable_4=lbound(x,4),ubound(x,4)
        call cp_store_real_vector(x(cp_loop_variable_1,
     +cp_loop_variable_2, cp_loop_variable_3,cp_loop_variable_4,:),
     +size(x(cp_loop_variable_1,cp_loop_variable_2, 
     +cp_loop_variable_3,cp_loop_variable_4,:)),
     +theArgFStack,
     +theArgFStackoffset,
     +theArgFStackSize)
             end do
             end do
           end do
        end do
      end subroutine 


      subroutine cp_arg_restore_real_five_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4,
     +cp_loop_variable_5)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
           do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
            do cp_loop_variable_3=ubound(x,3),lbound(x,3),-1
               do cp_loop_variable_4=ubound(x,4),lbound(x,4),-1
                  do cp_loop_variable_5=ubound(x,5),lbound(x,5),-1
                   x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4,cp_loop_variable_5)%v=
     +theArgFStack(theArgFStackoffset)
                   theArgFStackoffset=theArgFStackoffset-1
                  end do
               end do
            end do
           end do
        end do
        end subroutine 

      subroutine cp_res_store_real_five_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4,
     +cp_loop_variable_5)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1)
           do cp_loop_variable_2=lbound(x,2),ubound(x,2)
            do cp_loop_variable_3=lbound(x,3),ubound(x,3)
               do cp_loop_variable_4=lbound(x,4),ubound(x,4)
        call cp_store_real_vector(x(cp_loop_variable_1,
     +cp_loop_variable_2, cp_loop_variable_3,cp_loop_variable_4,:),
     +size(x(cp_loop_variable_1,cp_loop_variable_2, 
     +cp_loop_variable_3,cp_loop_variable_4,:)),
     +theResFStack,
     +theResFStackoffset,
     +theResFStackSize)
             end do
             end do
           end do
        end do
      end subroutine 


      subroutine cp_res_restore_real_five_tensor_a(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4,
     +cp_loop_variable_5)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
           do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
            do cp_loop_variable_3=ubound(x,3),lbound(x,3),-1
               do cp_loop_variable_4=ubound(x,4),lbound(x,4),-1
                  do cp_loop_variable_5=ubound(x,5),lbound(x,5),-1
                   x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4,cp_loop_variable_5)%v=
     +theResFStack(theResFStackoffset)
                   theResFStackoffset=theResFStackoffset-1
                  end do
               end do
            end do
           end do
        end do
        end subroutine 

C passive reals ----------------------------------------------
      subroutine cp_arg_store_real_scalar(x)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision :: x
C $OpenAD$ END DECLS
        call cp_store_real_scalar(x,theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
      end subroutine 


      subroutine cp_arg_restore_real_scalar(x)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision :: x
C $OpenAD$ END DECLS
        x=theArgFStack(theArgFStackoffset)
C        write(*,'(A,EN26.16E3)') "restore(s)  ", x
        theArgFStackoffset=theArgFStackoffset-1
      end subroutine 


      subroutine cp_res_store_real_scalar(x)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision :: x
C $OpenAD$ END DECLS
        call cp_store_real_scalar(x,theResFStack,theResFStackoffset,
     +theResFStackSize)
      end subroutine 


      subroutine cp_res_restore_real_scalar(x)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision :: x
C $OpenAD$ END DECLS
C        print*, "restore idx, value, x ", theResFStackoffset, x
        x=theResFStack(theResFStackoffset)
        theResFStackoffset=theResFStackoffset+1
      end subroutine 


      subroutine cp_arg_store_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        call cp_store_p_real_vector(x,size(x),
     +theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
      end subroutine 


      subroutine cp_arg_restore_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
           x(cp_loop_variable_1)=theArgFStack(theArgFStackoffset)
           theArgFStackoffset=theArgFStackoffset-1
C        write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +x(cp_loop_variable_1)
        end do
      end subroutine 


      subroutine cp_res_store_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        call cp_store_p_real_vector(x,size(x),
     +theResFStack,theResFStackoffset,
     +theResFStackSize)
      end subroutine 


      subroutine cp_res_restore_real_vector(x,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_1=lbound(x,1),ubound(x,1),1
           x(cp_loop_variable_1)=theResFStack(theResFStackoffset)
           theResFStackoffset=theResFStackoffset+1
        end do
      end subroutine 


      subroutine cp_arg_store_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_2=lbound(x,2),ubound(x,2)
        call cp_store_p_real_vector(x(:,cp_loop_variable_2),
     +size(x(:,cp_loop_variable_2)),theArgFStack,theArgFStackoffset,
     +theArgFStackSize)
        end do
      end subroutine 


      subroutine cp_arg_restore_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
           do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
            x(cp_loop_variable_1,cp_loop_variable_2)=
     +theArgFStack(theArgFStackoffset)
            theArgFStackoffset=theArgFStackoffset-1
           end do
        end do
      end subroutine 


      subroutine cp_res_store_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_2=lbound(x,2),ubound(x,2)
        call cp_store_p_real_vector(x(:,cp_loop_variable_2),
     +size(x(:,cp_loop_variable_2)),theResFStack,theResFStackoffset,
     +theResFStackSize)
        end do
      end subroutine 


      subroutine cp_res_restore_real_matrix(x,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_2=lbound(x,2),ubound(x,2),1
           do cp_loop_variable_1=lbound(x,1),ubound(x,1),1
            x(cp_loop_variable_1,cp_loop_variable_2)=
     +theResFStack(theResFStackoffset)
            theResFStackoffset=theResFStackoffset+1
           end do
        end do
      end subroutine 


      subroutine cp_arg_store_real_four_tensor(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_4=lbound(x,4),ubound(x,4)
           do cp_loop_variable_3=lbound(x,3),ubound(x,3)
            do cp_loop_variable_2=lbound(x,2),ubound(x,2)
        call cp_store_p_real_vector(x(:,cp_loop_variable_2,
     +cp_loop_variable_3, cp_loop_variable_4),
     +size(x(:,cp_loop_variable_2,cp_loop_variable_3, cp_loop_variable_4)),
     +theArgFStack,
     +theArgFStackoffset,
     +theArgFStackSize)
             end do
           end do
        end do
      end subroutine 


      subroutine cp_arg_restore_real_four_tensor(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_4=ubound(x,4),lbound(x,4),-1
           do cp_loop_variable_3=ubound(x,3),lbound(x,3),-1
            do cp_loop_variable_2=ubound(x,2),lbound(x,2),-1
               do cp_loop_variable_1=ubound(x,1),lbound(x,1),-1
                  x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4)=
     +theArgFStack(theArgFStackoffset)
                  theArgFStackoffset=theArgFStackoffset-1
               end do
            end do
           end do
        end do
        end subroutine 


      subroutine cp_res_store_real_four_tensor(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(::) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_4=lbound(x,4),ubound(x,4)
           do cp_loop_variable_3=lbound(x,3),ubound(x,3)
            do cp_loop_variable_2=lbound(x,2),ubound(x,2)
        call cp_store_p_real_vector(x(:,cp_loop_variable_2,
     +cp_loop_variable_3, cp_loop_variable_4),
     +size(x(:,cp_loop_variable_2,cp_loop_variable_3, cp_loop_variable_4)),
     +theResFStack,
     +theResFStackoffset,
     +theResFStackSize)
            end do
           end do
        end do
      end subroutine 


      subroutine cp_res_restore_real_four_tensor(x,cp_loop_variable_1,
     +cp_loop_variable_2,cp_loop_variable_3,cp_loop_variable_4)
C $OpenAD$ INLINE DECLS
        implicit none
        double precision, dimension(:) :: x
C $OpenAD$ END DECLS
        do cp_loop_variable_4=lbound(x,4),ubound(x,4)
           do cp_loop_variable_3=lbound(x,3),ubound(x,3)
            do cp_loop_variable_2=lbound(x,2),ubound(x,2)
               do cp_loop_variable_1=lbound(x,1),ubound(x,1)
                  x(cp_loop_variable_1,cp_loop_variable_2,
     +cp_loop_variable_3,cp_loop_variable_4)=
     +theResFStack(theResFStackoffset)
                  theResFStackoffset=theResFStackoffset+1
               end do
            end do
           end do
        end do
      end subroutine 


C integers -----------------------------------------------------
      subroutine cp_arg_store_integer_scalar(i)
C $OpenAD$ INLINE DECLS
        implicit none
        integer :: i
C $OpenAD$ END DECLS
        call cp_store_int_scalar(i,theArgIStack,
     +theArgIStackoffset, theArgIStackSize)
      end subroutine 


      subroutine cp_arg_restore_integer_scalar(i)
C $OpenAD$ INLINE DECLS
        implicit none
        integer :: i
C $OpenAD$ END DECLS
        i=theArgIStack(theArgIStackoffset)
C        write(*,'(A,I5,I5)') "restore(s)  ", i, theArgIStackOffset
        theArgIStackoffset=theArgIStackoffset-1
      end subroutine 


      subroutine cp_res_store_integer_scalar(i)
C $OpenAD$ INLINE DECLS
        implicit none
        integer :: i
C $OpenAD$ END DECLS
        call cp_store_int_scalar(i,theResIStack,
     +theResIStackoffset, theResIStackSize)
      end subroutine 


      subroutine cp_res_restore_integer_scalar(i)
C $OpenAD$ INLINE DECLS
        implicit none
        integer :: i
C $OpenAD$ END DECLS
        i=theResIStack(theResIStackoffset)
        theResIStackoffset=theResIStackoffset+1
      end subroutine 

      subroutine cp_arg_store_integer_vector(i,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        integer, dimension(:) :: i
C $OpenAD$ END DECLS
        call cp_store_int_vector(i,size(i),
     +theArgIStack,theArgIStackoffset,
     +theArgIStackSize)
      end subroutine 


      subroutine cp_arg_restore_integer_vector(i,cp_loop_variable_1)
C $OpenAD$ INLINE DECLS
        implicit none
        integer, dimension(:) :: i
C $OpenAD$ END DECLS
        do cp_loop_variable_1=ubound(i,1),lbound(i,1),-1
           i(cp_loop_variable_1)=theArgIStack(theArgIStackoffset)
           theArgIStackoffset=theArgIStackoffset-1
C        write(*,'(A,EN26.16E3)') "restore(v)  ", 
C     +i(cp_loop_variable_1)
        end do
      end subroutine 


      subroutine cp_arg_store_integer_matrix(i,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        integer, dimension(::) :: i
C $OpenAD$ END DECLS
        do cp_loop_variable_2=lbound(i,2),ubound(i,2)
        call cp_store_int_vector(i(:,cp_loop_variable_2),
     +size(i(:,cp_loop_variable_2)),theArgIStack,theArgIStackoffset,
     +theArgIStackSize)
        end do
      end subroutine 


      subroutine cp_arg_restore_integer_matrix(i,cp_loop_variable_1,
     +cp_loop_variable_2)
C $OpenAD$ INLINE DECLS
        implicit none
        integer, dimension(::) :: i
C $OpenAD$ END DECLS
        do cp_loop_variable_2=ubound(i,2),lbound(i,2),-1
           do cp_loop_variable_1=ubound(i,1),lbound(i,1),-1
            i(cp_loop_variable_1,cp_loop_variable_2)=
     +theArgIStack(theArgIStackoffset)
            theArgIStackoffset=theArgIStackoffset-1
           end do
        end do
      end subroutine 

C strings  -----------------------------------------------------
      subroutine cp_arg_store_string_scalar(s)
C $OpenAD$ INLINE DECLS
        implicit none
        character*(80) :: s
C $OpenAD$ END DECLS 
        call cp_store_string_scalar(s,theArgSStack,
     +theArgSStackoffset, theArgSStackSize)
      end subroutine 


      subroutine cp_arg_restore_string_scalar(s)
C $OpenAD$ INLINE DECLS
        implicit none
        character*(80) :: s
C $OpenAD$ END DECLS
        s=theArgSStack(theArgSStackoffset)
        theArgSStackoffset=theArgSStackoffset-1
      end subroutine 


      subroutine cp_res_store_string_scalar(s)
C $OpenAD$ INLINE DECLS
        implicit none
        character*(80) :: s
C $OpenAD$ END DECLS
        call cp_store_string_scalar(s,theResSStack,
     +theResSStackoffset, theResSStackSize)
      end subroutine 


      subroutine cp_res_restore_string_scalar(s)
C $OpenAD$ INLINE DECLS
        implicit none
        character*(80) :: s
C $OpenAD$ END DECLS
        s=theResSStack(theResSStackoffset)
        theResSStackoffset=theResSStackoffset+1
      end subroutine 


C bools  -----------------------------------------------------
      subroutine cp_arg_store_bool_scalar(b)
C $OpenAD$ INLINE DECLS
        implicit none
        logical :: b
C $OpenAD$ END DECLS
        call cp_store_bool_scalar(b,theArgBStack,
     +theArgBStackoffset, theArgBStackSize)
      end subroutine 


      subroutine cp_arg_restore_bool_scalar(b)
C $OpenAD$ INLINE DECLS
        implicit none
        logical :: b
C $OpenAD$ END DECLS
        b=theArgBStack(theArgBStackoffset)
        theArgBStackoffset=theArgBStackoffset-1
      end subroutine 


      subroutine cp_res_store_bool_scalar(b)
C $OpenAD$ INLINE DECLS
        implicit none
        logical :: b
C $OpenAD$ END DECLS
        call cp_store_bool_scalar(b,theResBStack,
     +theResBStackoffset, theResBStackSize)
      end subroutine 


      subroutine cp_res_restore_bool_scalar(b)
C $OpenAD$ INLINE DECLS
        implicit none
        logical :: b
C $OpenAD$ END DECLS
        b=theResBStack(theResBStackoffset)
        theResBStackoffset=theResBStackoffset+1
      end subroutine 
