!#########################################################
! This file is part of OpenAD released under the LGPL.   #
! The full COPYRIGHT notice can be found in the top      #
! level directory of the OpenAD distribution             #
!#########################################################
module OAD_trace

  use w2f__types

  implicit none        

! public members
  integer, public :: oad_trace_io_unit
  public :: oad_trace_init
  public :: oad_trace_open
  public :: oad_trace_close
  public :: oad_trace_call
  public :: oad_trace_ecall
  public :: oad_trace_arg
  public :: oad_trace_earg
  public :: oad_trace_index  
  public :: oad_trace_loop
  public :: oad_trace_eloop
  public :: oad_trace_branch
  public :: oad_trace_ebranch
  public :: oad_trace_initialization
  public :: oad_trace_einitialization
  public :: oad_trace_condition
  public :: oad_trace_econdition
  public :: oad_trace_update
  public :: oad_trace_eupdate
  public :: oad_trace_cfval  
  public :: oad_tan  

  interface oad_trace_init
     module procedure oad_trace_init_i
  end interface

  interface oad_trace_open
     module procedure oad_trace_open_i
  end interface

  interface oad_trace_close
     module procedure oad_trace_close_i
  end interface

  interface oad_trace_call
    module procedure oad_trace_call_is
    module procedure oad_trace_call_il
  end interface

  interface oad_trace_ecall
    module procedure oad_trace_ecall_i
  end interface

  interface oad_trace_arg
    module procedure oad_trace_arg_i
  end interface

  interface oad_trace_earg
    module procedure oad_trace_earg_i
  end interface

  interface oad_trace_index
    module procedure oad_trace_index_is
    module procedure oad_trace_index_il
  end interface

  interface oad_trace_loop
    module procedure oad_trace_loop_is
    module procedure oad_trace_loop_il
  end interface

  interface oad_trace_eloop
    module procedure oad_trace_eloop_i
  end interface

  interface oad_trace_branch
    module procedure oad_trace_branch_is
    module procedure oad_trace_branch_il
  end interface

  interface oad_trace_ebranch
    module procedure oad_trace_ebranch_i
  end interface

  interface oad_trace_initialization
    module procedure oad_trace_initialization_i
  end interface

  interface oad_trace_einitialization
    module procedure oad_trace_einitialization_i
  end interface

  interface oad_trace_condition
    module procedure oad_trace_condition_i
  end interface

  interface oad_trace_econdition
    module procedure oad_trace_econdition_i
  end interface

  interface oad_trace_update
    module procedure oad_trace_update_i
  end interface

  interface oad_trace_eupdate
    module procedure oad_trace_eupdate_i
  end interface

  interface oad_trace_cfval
    module procedure oad_trace_cfval_is
    module procedure oad_trace_cfval_il
  end interface

  interface oad_tan
    module procedure oad_tan_s
    module procedure oad_tan_d
  end interface

! private members
  integer, private :: fileNumber
  private :: oad_trace_findunit

  interface oad_trace_findunit
     module procedure trace_findunit_i
  end interface

  real(w2f__8), parameter :: Pi=3.141592653589793D0
  real(w2f__8), parameter :: PiHalf=Pi/2.0D0

contains

  subroutine oad_trace_init_i
    use OAD_rev
    implicit none
    fileNumber=1
    call OAD_revInit() ! this comes from OAD_rev
  end subroutine

  subroutine oad_trace_open_i()
    use OAD_rev
    implicit none
    character*128 fname ! file name
    ! set the mode
    our_rev_mode%plain=.FALSE.
    our_rev_mode%tape=.TRUE.
    ! get unit
    call oad_trace_findunit()
    print *, 'OAD: opening trace file ', fileNumber
    ! construct the file name
    write(fname,'(A,I3.3,A)') 'oad_tr_',fileNumber,'.xml'
    open( UNIT=oad_trace_io_unit,FILE=TRIM(fname),FORM='formatted',STATUS='UNKNOWN' )
    write(oad_trace_io_unit,'(A,I0,A)') '<Trace number="',fileNumber,'">'
    fileNumber=fileNumber+1
  end subroutine 

  subroutine oad_trace_close_i()
    use OAD_rev
    implicit none
    ! reset the mode
    our_rev_mode%plain=.TRUE.
    our_rev_mode%tape=.FALSE.
    write(oad_trace_io_unit,'(A,I0,A)') '</Trace>'
    close( UNIT=oad_trace_io_unit)
  end subroutine

  subroutine trace_findunit_i()
    ! returns a valid, unused unit number for Fortran I/O
    ! the routine stops the program if an error occurs in the process
    ! of searching the I/O channels.
    implicit none
    ! Local
    integer ii
    logical op
    integer ios
    character*(1024) msgbuf
    ! Sweep through a valid range of unit numbers
    oad_trace_io_unit=-1
    do ii=9,999
       if (oad_trace_io_unit.eq.-1) then
          inquire(unit=ii,iostat=ios,opened=op)
          if (ios.ne.0) then
             write(msgbuf,'(a,i2.2)')  'OAD: trace_findunit_i: inquiring unit number = ',ii
             print *, msgBuf
             write(msgbuf,'(a)') 'OAD: trace_findunit_i: inquire statement failed!'
             print *, msgBuf
             stop 'OAD: ERROR: in OAD_trace:trace_findunit_i'
          endif
          if (.NOT. op) then
             oad_trace_io_unit=ii
          end if
       end if
    end do
    ! Was there an available unit number
    if (oad_trace_io_unit.eq.-1) then
       write(msgbuf,'(a)')  'OAD: trace_findunit_i: could not find an available unit number!'
       print *, msgBuf
       stop 'OAD: ERROR: in OAD_trace:trace_findunit_i'
    endif
  end subroutine

  subroutine oad_trace_call_is (n,l) 
    character(*), intent(in) :: n
    integer(w2f__i4), intent(in) ::  l
    write(oad_trace_io_unit,'(A,A,A,I0,A)') '<Call name="', TRIM(n),'" line="',l,'">'
  end subroutine

  subroutine oad_trace_call_il (n,l) 
    character(*), intent(in) :: n
    integer(w2f__i8), intent(in) ::  l
    write(oad_trace_io_unit,'(A,A,A,I0,A)') '<Call name="', TRIM(n),'" line="',l,'">'
  end subroutine

  subroutine oad_trace_ecall_i () 
    write(oad_trace_io_unit,'(A)') '</Call>'
  end subroutine

  subroutine oad_trace_arg_i (n) 
    character(*), intent(in) :: n
    write(oad_trace_io_unit,'(A,A,A)') '<Arg name="', TRIM(n),'">'
  end subroutine

  subroutine oad_trace_earg_i () 
    write(oad_trace_io_unit,'(A)') '</Arg>'
  end subroutine

  subroutine oad_trace_index_is (i) 
    integer(w2f__i4), intent(in) :: i
    write(oad_trace_io_unit,'(A,I0,A)') '<Index val="',i,'"/>'
  end subroutine

  subroutine oad_trace_index_il (i) 
    integer(w2f__i8), intent(in) :: i
    write(oad_trace_io_unit,'(A,I0,A)') '<Index val="',i,'"/>'
  end subroutine

  subroutine oad_trace_loop_is (l) 
    integer(w2f__i4), intent(in) ::  l
    write(oad_trace_io_unit,'(A,I0,A)') '<Loop line="',l,'">'
  end subroutine

  subroutine oad_trace_loop_il (l) 
    integer(w2f__i8), intent(in) ::  l
    write(oad_trace_io_unit,'(A,I0,A)') '<Loop line="',l,'">'
  end subroutine

  subroutine oad_trace_eloop_i () 
    write(oad_trace_io_unit,'(A)') '</Loop>'
  end subroutine

  subroutine oad_trace_branch_is (l) 
    integer(w2f__i4), intent(in) ::  l
    write(oad_trace_io_unit,'(A,I0,A)') '<Branch line="',l,'">'
  end subroutine

  subroutine oad_trace_branch_il (l) 
    integer(w2f__i8), intent(in) ::  l
    write(oad_trace_io_unit,'(A,I0,A)') '<Branch line="',l,'">'
  end subroutine

  subroutine oad_trace_ebranch_i () 
    write(oad_trace_io_unit,'(A)') '</Branch>'
  end subroutine

  subroutine oad_trace_initialization_i () 
    write(oad_trace_io_unit,'(A)') '<Initialization>'
  end subroutine

  subroutine oad_trace_einitialization_i () 
    write(oad_trace_io_unit,'(A)') '</Initialization>'
  end subroutine

  subroutine oad_trace_condition_i () 
    write(oad_trace_io_unit,'(A)') '<Condition>'
  end subroutine

  subroutine oad_trace_econdition_i () 
    write(oad_trace_io_unit,'(A)') '</Condition>'
  end subroutine

  subroutine oad_trace_update_i () 
    write(oad_trace_io_unit,'(A)') '<Update>'
  end subroutine

  subroutine oad_trace_eupdate_i () 
    write(oad_trace_io_unit,'(A)') '</Update>'
  end subroutine

  subroutine oad_trace_cfval_is (i) 
    integer(w2f__i4), intent(in) :: i
    write(oad_trace_io_unit,'(A,I0,A)') '<Cfval val="',i,'"/>'
  end subroutine

  subroutine oad_trace_cfval_il (i) 
    integer(w2f__i8), intent(in) :: i
    write(oad_trace_io_unit,'(A,I0,A)') '<Cfval val="',i,'"/>'
  end subroutine

  real(w2f__8) function oad_tan_d(x)
    real(w2f__8) :: x
    oad_tan_d=tan(x)
    write(oad_trace_io_unit,'(A,I0,A)') '<Tan sd="',INT((x+PiHalf)/Pi),'"/>'
  end function

  real(w2f__4) function oad_tan_s(x)
    real(w2f__4) :: x
    oad_tan_s=tan(x)
    write(oad_trace_io_unit,'(A,I0,A)') '<Tan sd="',INT((x+PiHalf)/Pi),'"/>'
  end function

end module



