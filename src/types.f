module types_and_interfaces

	implicit none

	integer,parameter		:: sp = selected_real_kind(p=6,r=37)
	integer,parameter		:: dp = selected_real_kind(p=15,r=307)

	! provide interface to the objective function (genetic algorthim type)
	interface
		function objfun_ga(n, p) result(fun_val)
			implicit none
			integer, intent(in)		:: n
			real, intent(in)		:: p(:)
			real					:: fun_val
		end function objfun_ga
	end interface

	! interface to the function that calculates the signal
	interface fun
		real(kind=8) function fun (w)
			implicit none
			real(kind=8), intent(in)	:: w
		end function fun
	end interface fun

	! interface to the bcz component
	interface bcz_comp
		real(kind=8) function bcz_comp(w)
			implicit none
			real(kind=8), intent(in)	:: w
		end function bcz_comp
	end interface bcz_comp
	
	! interface to the helium component
	interface he_comp
		real(kind=8) function he_comp (w)
			implicit none
			real(kind=8), intent(in)	:: w
		end function he_comp
	end interface he_comp

	
	! interface to the parameter rescaling subroutine
	interface rescale
		subroutine rescale(array_in, array_out)
			implicit none
			real, dimension(:), intent(in)			:: array_in
			real(kind=8), dimension(:), intent(out) :: array_out
		end subroutine rescale
	end interface rescale
	
end module types_and_interfaces
