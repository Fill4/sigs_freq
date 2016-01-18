!*******************************************************************************
! Joao Faria: Jan 2013
!*******************************************************************************
  subroutine fitlamb (initial_lambda, chi2)
!    This subroutine iterates on various values of lambda to estimate the best
!    guess for the parameters. For each value of lambda we run a cycle where we 
!    remove a smooth function and then use a minimization algorithm to determine
!    the best parameters that represent the residuals, and repeating the process
!	 using the previously determined parameters to improve the smooth function 
! 	 until convergence is achieved.

	use types_and_interfaces
	use commonvar
	use commonarray
	
	use lib_io
	use lib_pikaia12
	use lib_array
	use lib_plot

	implicit none
	
	real(dp), intent(in)            :: initial_lambda
	real(dp), intent(inout)         :: chi2

	character(len=80)               :: outfile


	real    :: ctrl(12), x(nconst), f, rtol, c0(nconst)
	integer :: seed, exit_status, new_unit, i, iter, j, ii


	!! output file
	new_unit = next_unit()
	open (new_unit, file='lambda-chi2.dat', status='unknown', POSITION='APPEND')

	!Define seed for random number generator
	seed=TIME()
	!Initialize the random-number generator
	call rninit(seed)
	
	!Initialize c(:) and iter to 0
	do i=1,nconst
		c(i)=0
	end do
	iter = 1
	!Initial definition to enter cycle
	rtol = 1d0
	

	!Cycles through values of lambda from inital_lambda until lambda_max_iterations have been completed
	lambda = initial_lambda
	!Cycle updates the smooth function with last determined parameters c(:) until parameters
	!converge or smooth_max_iter is reached
	do while (rtol.gt.ftol .and. iter.le.smooth_iter_max)
		!Update c0 to determine rtol in the final part of the cycle to check for continuation in while
		j = 0
		do j=1,nconst
			c0(j)=c(j)
		end do
		rtol=0.0d0

		!Remove already determined function to initial frequencies to improve smooth fit.
		!On first run it doesn't remove anything because parameters c(:) are unknown 
		call subtract_and_smooth(lambda)

		! set control variables
		ctrl(1:12) = -1
		ctrl(1) = pikaia_pop
		ctrl(2) = pikaia_gen
		ctrl(5) = 5 ! one-point+creep, adjustable rate based on fitness
		!ctrl(12) = 2
		outfile = 'param_file'

		! now call pikaia
		CALL pikaia(objfun_ga, nconst, ctrl, x, f, exit_status)

		! rescaling parameters
		call rescale(x, c)

		! print lambda, chi^2 and parameters to file *********
		write(new_unit, '(es12.2, f10.2, 4f10.4)') lambda, 1.0/f, &
										   c(1)/(w0ref*fac), c(4)/(w0ref*fac), &
										   c(3), c(6) * (sin(c(7)))**2
		call flush(new_unit)
		!**************************************

		if (exit_status /= 0) then
			write(6,*) "Error in PIKAIA"
			stop
		endif

		do ii=1,nconst
	    	rtol=rtol+abs((c(ii)-c0(ii)))/max(1.0d0,abs(c(ii)+c0(ii)))
		end do

		print *,rtol

		iter = iter + 1
	end do

	if (iprint.ge.1) write (3,*) ' '


	if (iprint.ge.3) call writeout (1)

	if (iprint.ge.1) write (6,*) ' '

	chi2 = 1.0_dp / f

	close(new_unit)
	
	return

  end subroutine fitlamb
  
  
  
  function objfun_ga(npar, p) result(fun_val)
! This function calculates the objective function value, i.e. the chi^2.
! The signal with the current parameters P is subtracted from the frequencies
! and the result is smoothed by a polynomial before calculating the chi^2
	
	use types_and_interfaces, only: dp, fun, rescale
	use commonvar, only : iprint, use_error_chi2
	use commonarray
	
	implicit none
	
	integer, intent(in) :: npar ! size of parameter space
	real, intent(in)    :: p(:)
	real                :: fun_val
	
	real(dp)    :: ww, sf, resid
	integer     :: i
	
	! rescaling parameters
	call rescale(p, c)

	resid = 0.0d0
	! if using errors -
	if (use_error_chi2) then
		do i=1,n
			ww = w(i)
			sf = fun(ww)
			resid = resid + ((sd(i)-sf)/sig(i))**2
		end do
	! if not using errors -
	else if (.not. use_error_chi2) then
		do i=1,n
			ww = w(i)
			sf = fun(ww)
			resid = resid + (sd(i)-sf)**2
		end do
	end if

	fun_val = sngl(1.0 / resid)

  end function objfun_ga



  subroutine rescale(array_in, array_out)
		
		use types_and_interfaces, only: dp
		use commonvar, only: pi, w0ref, fac
		
		implicit none
		
		real, dimension(:), intent(in)      :: array_in
		real(dp), dimension(:), intent(out) :: array_out

		array_out(1) = dble(array_in(1)) * (3000.*w0ref*fac - 2000.*w0ref*fac) + 2000.*w0ref*fac
		array_out(2) = dble(array_in(2)) * pi
		array_out(3) = dble(array_in(3))
		
		array_out(4) = dble(array_in(4)) * (800.*w0ref*fac - 500.*w0ref*fac) + 500.*w0ref*fac
		array_out(5) = dble(array_in(5)) * pi
		array_out(6) = dble(array_in(6)) * 5.0_dp
		array_out(7) = dble(array_in(7)) * (300.*w0ref*fac - 100.*w0ref*fac) + 100.*w0ref*fac
  
  
  end subroutine rescale
