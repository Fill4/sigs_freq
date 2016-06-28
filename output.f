!*******************************************************************************
! João Faria: Feb 2013
!*******************************************************************************
  subroutine output (afile, chi2)
!   write the OUTPUT

	use types_and_interfaces, only: dp, fun, he_comp, bcz_comp
	use commonvar
	use commonarray, only: c, w, sd, sig, n, l
	use lib_io
	use lib_array
	use lib_plot
	implicit none

	character(len=80), intent(in) :: afile
	real(dp), intent(inout)	:: chi2
	real(dp)				:: tau_bcz, tau_he, beta
	real(dp)				:: a_bcz, a_he
	real(dp), dimension(150)	:: xx, resultfun, result_he, result_bcz
	real(dp)					:: min_xx, max_xx
	integer :: nfile, length, new_unit, i

	! parameters that are in seconds need to be converted
	tau_bcz = c(1) / (w0ref*fac)
	tau_he = c(4) / (w0ref*fac)
	beta = c(7) / (w0ref*fac)
	a_bcz = c(3)
	a_he = c(6) * (sin(c(7)))**2

	if (verbose) then
		write (6,*) "  Frequencies from file: ", afile
		write (6,1010)	'Results:', &
						'tau_BCZ = ', tau_bcz, 'Phi = ', c(2), &
						'A_BCZ = ', a_bcz, &
						'tau_HeII = ', tau_he, 'Phi = ', c(5), &
						'A_HeII = ', a_he, 'beta_HeII = ', beta, &
						'chi2 = ', chi2, 'chi2norm = ', chi2/(n-nconst)
	end if

1010 format (3x, a, //, &
			&3x, a, f9.4, 6x, a, f10.6, //,&
			&3x, a, f9.2, //,&
			&3x, a, f9.4, 6x, a, f10.6, //,&
			&3x, a, f9.2, 6x, a, f10.4, //,&
			&3x, a, f12.5, 3x, a, f10.5 //)


	! ploting the results
	!   create array with smooth function
	min_xx = minval(w(1:n))! - 1.0d-4
	max_xx = maxval(w(1:n))! + 1.0d-4
	call linspace(min_xx, max_xx, xx)
	do i=1,150
		resultfun(i) = fun(xx(i))
		result_bcz(i) = bcz_comp(xx(i))
		result_he(i) = he_comp(xx(i))
	end do
	
	if (show_plots) then
		call plot(xx*w0ref, result_bcz, &
			xx*w0ref, result_he, &
			' 5.00-',color2='black',color1='green')!, &
			!terminal='png')
			!yrange=(/-3.0d0,3.0d0/) )

		call plot(dble(w(1:n)*w0ref), dble(sd(1:n)), xx*w0ref, resultfun, &
			' 5.00-',color2='black',color1='green', &
			errors=dble(sig(1:n)) )!, &
			!terminal='png')
			!yrange=(/-3.0d0,3.0d0/) )                  
	endif

	! output parameters to "res" file -
	write (9,9003) afile, tau_bcz, c(2), a_bcz, tau_he, c(5), a_he, beta
9003	format (a24, 7f10.4)
	close(9)
	
	return
  
end subroutine output
