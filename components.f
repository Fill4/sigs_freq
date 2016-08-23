!*******************************************************************************
real(dp) function bcz_comp (nu)
!  this is the signal produced by the sharp transition in the base of the 
!  convection zone
!
!  see Monteiro et al. (1994), eq (20)
!  and Mazumdar et al. (2012), eq (2)

	use types_and_interfaces, only: dp
	use commonarray, only: c
	use commonvar, only: w0ref, pi
		
	implicit none

	real(dp), intent(in)    :: nu
	real(dp)  :: xarg, bcz


	! c(1) = tau_bcz
	! c(2) = phi_bcz
	! c(3) = amp_bcz
	! c(4) = tau_he
	! c(5) = phi_he
	! c(6) = amp_he
	! c(7) = width_he

	xarg = 4.0_dp*pi*c(1)*nu*w0ref*1.0d-6 + 2.0_dp*c(2)
	bcz  = ( c(3)/nu**2 ) * sin(xarg)
	
	bcz_comp = bcz

	return

end function bcz_comp

  
!*******************************************************************************
real(dp) function he_comp (nu)
!  this is the signal from the helium ionization region
!
!  see Mazumdar et al. (2012), eq (3)

	use types_and_interfaces, only: dp
	use commonarray, only: c
	use commonvar, only: w0ref, pi
		
	implicit none

	real(dp), intent(in) :: nu
	real(dp)  :: yarg, he


	! c(1) = tau_bcz
	! c(2) = phi_bcz
	! c(3) = amp_bcz
	! c(4) = tau_he
	! c(5) = phi_he
	! c(6) = amp_he
	! c(7) = width_he

	yarg = 4.0_dp*pi*c(4)*nu*w0ref*1.0d-6 + 2.0_dp*c(5)
	he = ( c(6)/nu ) * (sin(2.0_dp*pi*c(7)*nu*w0ref*1.0d-6))**2 * cos(yarg)
	
	he_comp = he

	return

end function he_comp
