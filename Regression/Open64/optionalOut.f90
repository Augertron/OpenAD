subroutine foo(o,m)

integer m
integer , optional :: o 

if (present(o)) then 
  print *,' in foo, input o is :', o
  o=m 
end if 

end subroutine

program test 
interface foo
subroutine foo(o,m)

integer m
integer , optional :: o
end subroutine
end interface
integer am,ao
am=2
call foo(m=am)
ao=1
call foo(o=ao,m=am)
print *, 'after foo ao is: ', ao
end program
