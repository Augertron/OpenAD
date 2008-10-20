! ========== begin copyright notice ==============
! This file is part of
! ---------------
! xaifBooster
! ---------------
! Distributed under the BSD license as follows:
! Copyright (c) 2005, The University of Chicago
! All rights reserved.
!
! Redistribution and use in source and binary forms,
! with or without modification, are permitted provided that the following conditions are met:
!
!    - Redistributions of source code must retain the above copyright notice,
!      this list of conditions and the following disclaimer.
!    - Redistributions in binary form must reproduce the above copyright notice,
!      this list of conditions and the following disclaimer in the documentation
!      and/or other materials provided with the distribution.
!    - Neither the name of The University of Chicago nor the names of its contributors
!      may be used to endorse or promote products derived from this software without
!      specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
! EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
! OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
! SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
! General Information:
! xaifBooster is intended for the transformation of
! numerical programs represented as xml files according
! to the XAIF schema. It is part of the OpenAD framework.
! The main application is automatic
! differentiation, i.e. the generation of code for
! the computation of derivatives.
! The following people are the principal authors of the
! current version:
!       Uwe Naumann
!       Jean Utke
! Additional contributors are:
!       Andrew Lyons
!       Peter Fine
!
! For more details about xaifBooster and its use in OpenAD please visit:
!   http://www.mcs.anl.gov/openad
!
! This work is partially supported by:
!       NSF-ITR grant OCE-0205590
! ========== end copyright notice ==============

      module OAD_graph
        
        use w2f__types
        implicit none
       !call tree nodes 
        public
        type vertex
          character (len = 40) :: value = ''
          integer :: doubles = 0
          integer :: integers = 0
          integer :: argFloats = 0
          integer :: argInts = 0
          integer :: argBools = 0
          type (list), pointer :: first => NULL()
        end type vertex

        type list
          type (vertex), pointer :: called => NULL()
          type (list), pointer :: next => NULL()
        end type list

       !call graph nodes
        type vertexlist
          character (len = 40) :: value = ''
          integer :: doubles = 0
          integer :: integers = 0
          integer :: argFloats = 0
          integer :: argInts = 0
          integer :: argBools = 0
          type (vertexlist), pointer :: next => NULL()
          type (edge), pointer :: calls => NULL()
        end type

        type edge
          character (len = 40) :: tofunc = ''
          integer :: traversed = 0
          type (edge), pointer :: next => NULL()
        end type
        

        public :: tree, cur, graph
        type (vertex),save:: tree
        type (list), pointer :: cur => NULL()
        type (vertexlist),save, target :: graph
         
        integer iaddr
        external iaddr
         
        interface makelink
          module procedure makelinks
        end interface

        interface printing
          module procedure graphprint
        end interface

        interface makegraphs
          module procedure makegraph
        end interface

        interface makevertexs
          module procedure makevertex
        end interface

        contains

        subroutine makelinks(srname, prev2)
            character(*) :: srname
            type (list), pointer :: prev2
            type (list), pointer :: newli, newli2
            type (vertex), pointer :: newel
 
            if (tree%value.eq.'') then
              tree%value = srname
              allocate(newli)
              tree%first => newli
              cur => newli
!              print *,iaddr(cur), 'top list'
!              print *,iaddr(cur%called), 'called' !%value
              
            else 
              
              if(.not. associated(cur%called)) then
                prev2 => cur
                allocate(newel)
                allocate(newli)
!                print *,iaddr(cur%called), 'called' !%value
                
                cur%called => newel
!                print *,iaddr(cur%called), 'called'
                
                newel%first => newli
                cur => newli
                newel%value = srname
!                print *, iaddr(cur), 'list start'!%value
!                print *,iaddr(prev2%called), 'called'
                
              else
                    allocate(newel)
                    allocate(newli)
                    allocate(newli2)
!                    print *,iaddr(cur), 'loc' !%value
                    
                    cur%next => newli
                    newli%called => newel
                    newel%value = srname

                    cur=> newli
                    prev2 => cur
                    newel%first => newli2
                    cur => newli2
!                    print *,iaddr(cur%called), 'called'
!                   print *,iaddr(cur), 'location of next list element'!%value
                    
              end if              
            end if


        end subroutine makelinks

        recursive subroutine graphprint(invertex)
          type (vertex) :: invertex
          integer five, ierror
          type (list), pointer :: itr 
          type (vertexlist), pointer :: itr2
          type (edge), pointer :: itr3
          type (edge), pointer :: newedge
          type (vertexlist), pointer :: newver
          character (len = 20) itoa
          character (len = 20) itoa2
          character (len = 20) itoa3
          character (len = 20) itoa4
          character (len = 20) itoa5
          character (len = 40) blankstr
          if(associated(invertex%first%next)) then
            itr => invertex%first
            do 100
              call makegraph(invertex, itr%called%value)
              write(itoa, '(I10)') itr%called%doubles
              itoa = adjustl(itoa)
              write(itoa2, '(I10)') itr%called%integers
              itoa2 = adjustl(itoa2)
              write(itoa3, '(I10)') itr%called%argFloats
              itoa3 = adjustl(itoa3)
              write(itoa4, '(I10)') itr%called%ArgInts
              itoa4 = adjustl(itoa4)
              write(itoa5, '(I10)') itr%called%ArgBools
              itoa5 = adjustl(itoa5)
              write(10,'(Z8,A,A,A,A,A,A,A)')iaddr(itr%called),&
              '[shape="box" height=.25 label="',&
       trim(itr%called%value),' ', trim(itoa), ':', trim(itoa2), '"];'
            write (10,'(Z8, A,Z8, A,A,A,A,A,A,A)') iaddr(invertex), '->',&
       iaddr(itr%called), '[label="', trim(itoa3), ':', trim(itoa4),&
       ':', trim(itoa5), '"];' 
              call graphprint(itr%called)
              if(associated (itr%next)) then
                itr => itr%next
             else
                call makevertex(itr%called)
                exit
              end if
            100 continue
          else
            if(associated(invertex%first%called)) then
              call makegraph(invertex, invertex%first%called%value)
              call makevertex(invertex%first%called)
              write(itoa, '(I10)') invertex%first%called%doubles
              itoa = adjustl(itoa)
              write(itoa2, '(I10)') invertex%first%called%integers
              itoa2 = adjustl(itoa2)
              write(itoa3, '(I10)') invertex%first%called%argFloats
              itoa3 = adjustl(itoa3)
              write(itoa4, '(I10)') invertex%first%called%ArgInts
              itoa4 = adjustl(itoa4)
              write(itoa5, '(I10)') invertex%first%called%ArgBools
              itoa5 = adjustl(itoa5)
            write(10,'(Z8,A,A,A,A,A,A,A)')iaddr(invertex%first%called),&
       '[shape="box" height=.25 label="',&
       trim(invertex%first%called%value), ' ', trim(itoa), ':',&
       trim(itoa2), '"];'
              write (10,'(Z8, A,Z8, A,A,A,A,A,A,A)') iaddr(invertex), '->',&
       iaddr(invertex%first%called), '[label="', trim(itoa3), ':',&
       trim(itoa4), ':', trim(itoa5), '"];'

               call graphprint(invertex%first%called)
            else
               call makevertex(invertex)
            endif
          end if
        end subroutine graphprint

        subroutine graph2print()
          type (vertexlist), pointer :: itr
          type (edge), pointer :: itr2
          character (len = 20) :: itoa
          character (len = 20) :: itoa2
          character (len = 20) :: itoa3
          character (len = 20) :: itoa4
          character (len = 20) :: itoa5          
          itr => graph
          do 130
            write(itoa, '(I10)') itr%doubles
            itoa = adjustl(itoa)
            write(itoa2, '(I10)') itr%integers
            itoa2 = adjustl(itoa2)
            write(itoa3, '(I10)') itr%argInts
            itoa3 = adjustl(itoa3)
            write(itoa4, '(I10)') itr%argFloats
            itoa4 = adjustl(itoa4)
            write(itoa5, '(I10)') itr%argBools
            itoa5 = adjustl(itoa5)
            write(11, '(A,A,A,A,A,A,A,A,A,A,A,A,A,A)') trim(itr%value),&
       '[shape="box" height=.25 label="', trim(itr%value),&
       ' ', trim(itoa), ':', trim(itoa2), ' ', trim(itoa4), ':',&
       trim(itoa3), ':', trim(itoa5),   '"];'
            if (associated(itr%next)) then
              itr => itr%next
            else
              exit
            end if               
          130 continue 
          itr =>graph          
          do 140
            if(associated(itr%calls)) then
              itr2 => itr%calls
              do 150
                write(itoa, '(I10)') itr2%traversed
                itoa = adjustl(itoa)                            
                write(11, *) trim(itr%value), '->', trim(itr2%tofunc),&
        '[label="', trim(itoa), '"];'
                if(associated(itr2%next)) then
                  itr2=>itr2%next
                else
                  exit
                end if
              150 continue
              if(associated(itr%next)) then
                itr=>itr%next
              else
                exit
              end if
            else
              if(associated(itr%next)) then
                itr=>itr%next
              else
                exit
              end if
            end if
         140 continue   
        end subroutine

        subroutine makegraph(invertex, itrcall)
          type (vertex) :: invertex
          character (len = 40) :: itrcall 
          type (vertexlist), pointer :: itr2
          type (edge), pointer :: itr3
          type (edge), pointer :: newedge
          type (vertexlist), pointer :: newver
          character (len=40) strblank
          itr2 => graph
          do 110
                if (itr2%value .eq. invertex%value) then
                  exit
                else
                  if(associated(itr2%next)) then
                    itr2 => itr2%next
                  else
                    allocate(newver)
                    itr2%next => newver
                    itr2 => newver
                    itr2%value = invertex%value
                    itr2%doubles = invertex%doubles
                    itr2%integers = invertex%integers
                    itr2%argInts = invertex%argInts
                    itr2%argFloats = invertex%argFloats
                    itr2%argBools = invertex%argBools
                    exit
                  endif
                endif
              110 continue
              if(associated(itr2%calls))then
                itr3 => itr2%calls
                do 120
                  if(itr3%tofunc .eq. itrcall) then
                    itr3%traversed = itr3%traversed + 1
                    exit
                  else
                    if (associated(itr3%next)) then
                      itr3 => itr3%next
                    else
                      allocate(newedge)
                      itr3%next => newedge
                      newedge%tofunc = itrcall
                      newedge%traversed = 1
                      exit
                    end if
                  end if
                120 continue
              else
                allocate(newedge)
                itr2%calls => newedge
                newedge%tofunc = itrcall
                newedge%traversed = 1
              end if
      end subroutine

      subroutine makevertex(invertex)
        type(vertex) :: invertex
        type (vertexlist), pointer :: itr2
        type (vertexlist), pointer::newver
         itr2 => graph
          do 110
                if (itr2%value .eq. invertex%value) then
                  exit
                else
                  if(associated(itr2%next)) then
                    itr2 => itr2%next
                  else
                    allocate(newver)
                    itr2%next => newver
                    itr2 => newver
                    itr2%value = invertex%value
                    itr2%doubles = invertex%doubles
                    itr2%integers = invertex%integers
                    itr2%argInts = invertex%argInts
                    itr2%argFloats = invertex%argFloats
                    itr2%argBools = invertex%argBools       
                    exit
                  endif
                endif
              110 continue

      end subroutine


      end module OAD_graph

