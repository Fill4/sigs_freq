!****************************************************************************
!	Filipe Pereira - Mar 2016
!****************************************************************************
subroutine rescale(array_in, array_out)
		
	use types_and_interfaces, only: dp
	use commonvar
	
	implicit none
	
	real, dimension(:), intent(in)      :: array_in
	real(dp), dimension(:), intent(out) :: array_out

	array_out(1) = dble(array_in(1)) * (upper_tau_bcz - lower_tau_bcz) &
												+ lower_tau_bcz
	array_out(2) = dble(array_in(2)) * pi
	array_out(3) = dble(array_in(3)) * 5.0_dp
	
	array_out(4) = dble(array_in(4)) * (upper_tau_he2 - lower_tau_he2) &
												+ lower_tau_he2
	array_out(5) = dble(array_in(5)) * pi
	array_out(6) = dble(array_in(6)) * 5.0_dp
	array_out(7) = dble(array_in(7)) * 300

end subroutine rescale