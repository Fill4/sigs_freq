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

	character(len=80) :: afile, filename

	real(dp), intent(in)  :: chi2
	real(dp)              :: chi2norm
	real(dp)              :: tau_bcz, tau_he, beta
	real(dp)              :: a_bcz, a_he
	
	real(dp), dimension(150)  :: xx, resultfun, result_he, result_bcz
	real(dp)                  :: min_xx, max_xx

	integer :: nfile, length, new_unit, i

	! parameters that are in seconds need to be converted
	tau_bcz = c(1) / (w0ref*fac)
	tau_he = c(4) / (w0ref*fac)
	beta = c(7) / (w0ref*fac)
	
	a_bcz = c(3)
	a_he = c(6) * (sin(c(7)))**2
	
	chi2norm = chi2 / (n-nconst)

	write (6,*) "  Frequencies from file: ", afile
	
	write (6,1010) 'Results:', &
						'tau_BCZ = ', tau_bcz, 'Phi = ', c(2), &
						'A_BCZ = ', a_bcz, &
						'tau_HeII = ', tau_he, 'Phi = ', c(5), &
						'A_HeII = ', a_he, 'beta_HeII = ', beta, &
						'chi2 = ', chi2, 'chi2norm = ', chi2norm

 1010   format (3x, a, //, &
				6x, a11, f10.2, 6x, a, f8.5, //, &
				6x, a11, f10.4, //, &
				6x, a11, f10.2, 6x, a, f8.5, //, &
				6x, a11, f10.4, 6x, a, f8.2, //, &
				//, &
				6x, a, f15.3, 5x, a, f15.3, /)


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
	write (9,9003) &
		   afile, tau_bcz, c(2), a_bcz, tau_he, c(5), a_he, beta
 9003   format (x, a24, 7f10.4)
	close(9)
	
	
	! output signal to file -
	!if (write_final) then
	!    write(filename, '("signal_",i4,"-",i0.4,".dat")') int(tau_bcz), int(tau_he)
	!    write(*,*) filename
	!    new_unit = next_unit()
	!    open (new_unit, file=filename, status='unknown')
	!    write(new_unit,'(a)') '# observed frequencies after smoothing'
	!    write(new_unit,'(a)') '# N'
	!    write(new_unit,'(a, x, a, 3a12)') '#', 'l', 'nu(muHz)', 'sd(muHz)', 'err(muHz)'
	!    write(new_unit,'(i3)') n
	!    write(new_unit,'(i3, 3f12.4)') (l(i), w(i)*w0ref, sd(i), sig(i), i=1,n)
	!    write(new_unit,'(a)') '# fitted signals'
	!    write(new_unit,'(a, x, 4a12)') '#', 'nu(muHz)', 'bcz', 'he', 'sum'
	!    write(new_unit,'(2x, 4f12.5)') (xx(i)*w0ref, result_bcz(i), result_he(i), resultfun(i), i=1,150)
	!    close(new_unit)
	!endif
	

	return
  
  end subroutine output
