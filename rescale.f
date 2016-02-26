subroutine rescale(array_in, array_out)
		
	use types_and_interfaces, only: dp
	use commonvar
	
	implicit none
	
	real, dimension(:), intent(in)      :: array_in
	real(dp), dimension(:), intent(out) :: array_out

	array_out(1) = dble(array_in(1)) * (upper_tau_bcz*w0ref*fac - lower_tau_bcz*w0ref*fac) &
												+ lower_tau_bcz*w0ref*fac
	array_out(2) = dble(array_in(2)) * pi
	array_out(3) = dble(array_in(3))
	
	array_out(4) = dble(array_in(4)) * (upper_tau_he2*w0ref*fac - lower_tau_he2*w0ref*fac) &
												+ lower_tau_he2*w0ref*fac
	array_out(5) = dble(array_in(5)) * pi
	array_out(6) = dble(array_in(6)) * 5.0_dp
	array_out(7) = dble(array_in(7)) * (upper_beta*w0ref*fac - lower_beta*w0ref*fac) &
												+ lower_beta*w0ref*fac

end subroutine rescale

subroutine set_rescale_values(iter)

	use types_and_interfaces, only: dp
	use commonvar
	use commonarray

	implicit none

	integer, intent(in)		:: iter
	real(dp)				:: interval_size(3)
	real(dp)				:: interval

	interval_size = (/ 1000, 750, 500/)

	!In the case of iter = 0 it's the first time the function is called.
	!Values for the rescale variables are defined according to star data.
	if (iter .eq. 0) then
		upper_tau_bcz = 3000
		lower_tau_bcz = 1500
		upper_tau_he2 = 1500
		lower_tau_he2 = 500
		upper_beta = 300
		lower_beta = 100
	!In the case of iter =! 0 we are calling a recalculation of the parameters.
	!This recalculation is based on the results obtained from the previous attempt
	else
		interval = interval_size(iter)
		upper_tau_bcz = (c(1)/(w0ref*fac))+(interval/2)
		lower_tau_bcz = (c(1)/(w0ref*fac))-(interval/2)
		upper_tau_he2 = (c(4)/(w0ref*fac))+(interval/2)
		lower_tau_he2 = (c(4)/(w0ref*fac))-(interval/2)
		upper_beta = c(7)/(w0ref*fac)+(interval/10)
		lower_beta = c(7)/(w0ref*fac)-(interval/10)
	endif

end subroutine set_rescale_values