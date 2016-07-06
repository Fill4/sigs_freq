!****************************************************************************
!	Filipe Pereira - Mar 2016
!****************************************************************************
subroutine rescale(array_in, array_out)
		
	use types_and_interfaces, only: dp
	use commonvar
	
	implicit none
	
	real, dimension(:), intent(in)      :: array_in
	real(dp), dimension(:), intent(out) :: array_out

	array_out(1) = dble(array_in(1)) * (upper_tau_bcz*w0ref*fac - lower_tau_bcz*w0ref*fac) &
												+ lower_tau_bcz*w0ref*fac
	array_out(2) = dble(array_in(2)) * pi
	array_out(3) = dble(array_in(3)) * 5.0_dp
	
	array_out(4) = dble(array_in(4)) * (upper_tau_he2*w0ref*fac - lower_tau_he2*w0ref*fac) &
												+ lower_tau_he2*w0ref*fac
	array_out(5) = dble(array_in(5)) * pi
	array_out(6) = dble(array_in(6)) * 5.0_dp
	array_out(7) = dble(array_in(7)) * (300*w0ref*fac - 0*w0ref*fac) &
												+ 0*w0ref*fac

end subroutine rescale