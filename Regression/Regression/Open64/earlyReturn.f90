 	function bar(x)
	integer x
	integer bar
	if (x .gt. 0) then
	  bar=1
	  return 
	else 
	  bar=2
	  return 
	end if 
	end function

	subroutine foo()
	integer a
	a=1
	if (a .eq. 0 ) then 
	  return 
        else 
	  print *, a 
	end if 
	end 

	program test
	call foo()
	end program
