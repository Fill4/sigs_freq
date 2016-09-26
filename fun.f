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

subroutine rescale(array_in, array_out)
! Rescales the parameters that come out of pikaia ARRAY_IN to their physical values ARRAY_OUT

	use types_and_interfaces, only: dp
	use commonvar
	
	implicit none
	
	real, dimension(:), intent(in)      :: array_in
	real(dp), dimension(:), intent(out) :: array_out

	! Bcz
	array_out(1) = dble(array_in(1)) * (upper_tau_bcz - lower_tau_bcz) + lower_tau_bcz
	array_out(2) = dble(array_in(2)) * pi
	array_out(3) = dble(array_in(3)) * 0.3_dp
	
	! HeII
	array_out(4) = dble(array_in(4)) * (upper_tau_he2 - lower_tau_he2) + lower_tau_he2
	array_out(5) = dble(array_in(5)) * pi
	array_out(6) = dble(array_in(6)) * (1.5_dp - 0.3_dp) + 0.3_dp
	array_out(7) = dble(array_in(7)) * 300

end subroutine rescale