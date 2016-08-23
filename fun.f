!**********************************************************
  real(dp) function fun (nu)
!  this is the function to be fitted. It corresponds to the the signal
!  produced by the sharp transition in the base of the convection zone
!  added to the one at the helium ionization region
!
!  see Monteiro et al. (1994), eq (20)
!  and Mazumdar et al. (2012), eq (2), (3)

	use types_and_interfaces, only: dp
	use commonarray, only: c
	use commonvar, only: w0ref, pi
	implicit none

	real(dp), intent(in)    :: nu
	real(dp)  :: xarg, yarg, bcz, he
	
	! c(1) = tau_bcz
	! c(2) = phi_bcz
	! c(3) = amp_bcz
	! c(4) = tau_he
	! c(5) = phi_he
	! c(6) = amp_he
	! c(7) = nuidth_he

	xarg = 4.0_dp*pi*c(1)*nu*w0ref*1.0d-6 + 2.0_dp*c(2)
	bcz  = ( c(3)/nu**2 ) * sin(xarg)

	yarg = 4.0_dp*pi*c(4)*nu*w0ref*1.0d-6 + 2.0_dp*c(5)
	he = ( c(6)/nu ) * (sin(2.0_dp*pi*c(7)*nu*w0ref*1.0d-6))**2 * cos(yarg)
	
	fun = bcz + he

	return
  
end function fun

