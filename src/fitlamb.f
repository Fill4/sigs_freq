subroutine fitlamb
! This subroutine iterates on various values of lambda to estimate the best
! guess for the parameters. For each value of lambda we run a cycle where we 
! remove a smooth function and then use a minimization algorithm to determine
! the best parameters that represent the residuals, and repeating the process
! using the previously determined parameters to improve the smooth function 
! until convergence is achieved.

	use types_and_interfaces
	use commonvar
	use commonarray

	use lib_pikaia12

	implicit none
	
	real(dp)	:: lambda_list(7), c0(nconst)
	real    	:: ctrl(12), x(nconst), f, rtol
	integer 	:: seed, exit_status, i, iter, j, ii

	lambda_list = (/ 1E-4, 5E-5, 1E-5, 5E-6, 1E-6, 5E-7, 1E-7/)

	!Define seed for random number generator
	seed=TIME()
	!Initialize the random-number generator
	call rninit(seed)

	!Select lambda_n according to number of available frequencies
	! TODO: Cycle for lambda not working! Change after testing rest of code.
	if (lambda==-1) then 
		select case (n)
			case (60 :)
				lambda = lambda_list(6)
			case (40:59)
				lambda = lambda_list(4)
			case (: 39)
				lambda = lambda_list(2)
		end select
	endif

	!Initialize c(:) and iter to 1
	do i=1,nconst
		c(i)=0
		x(i)=0
	end do
	iter = 1
	!Initial definition to enter cycle
	rtol = 100

	!Cycle updates the smooth function with last determined parameters c(:) until parameters
	!converge or smooth_max_iter is reached
	do while (rtol.gt.ftol .and. iter.le.smooth_iter_max)
		!Update previous iteration parameters to calculate new ones
		j = 0
		do j=1,nconst
			c0(j)=c(j)
		end do
		!Reset rtol
		rtol=0.0d0

		!Remove already determined function to initial frequencies to improve smooth fit.
		!On first run it doesn't remove anything because parameters c(:) are unknown 
		call subtract_and_smooth(lambda)

		!Set control variables
		ctrl(1:12) = -1
		ctrl(1) = pikaia_pop
		ctrl(2) = pikaia_gen
		ctrl(5) = 5 ! one-point+creep, adjustable rate based on fitness

		! now call pikaia
		CALL pikaia(objfun_ga, nconst, ctrl, x, f, exit_status)

		! rescaling parameters
		call rescale(x, c)

		!Check exit_status for errors during PIKAIA execution and stop if there were any
		if (exit_status /= 0) then
			write(6,*) "Error in PIKAIA"
			stop
		endif

		!Determine rtol to measure parameter change between iterations
		do ii=1,nconst
			rtol=rtol+abs((c(ii)-c0(ii)))/max(1.0d0,abs(c(ii)+c0(ii)))
		end do

		!increase iteration number
		iter = iter + 1
	end do

	if (verbose) write(6,*) ' '
	!Value for the chi squared obtained for the final parameters obtained
	chi2 = 1.0_dp / f
	close(3)
	
	return

end subroutine fitlamb
  
function objfun_ga(npar, p) result(fun_val)
! This function calculates the objective function value, i.e. the chi^2.
! The signal with the current parameters P is subtracted from the frequencies
! and the result is smoothed by a polynomial before calculating the chi^2
	
	use types_and_interfaces, only: dp, fun, rescale
	use commonvar, only : use_error_chi2
	use commonarray
	
	implicit none
	
	integer, intent(in) 	:: npar
	real, intent(in)    	:: p(:)
	real                	:: fun_val
	
	real(dp)    :: ww, sf, resid, nn, err_mult
	integer     :: i
	
	! Rescaling parameters
	call rescale(p, c)

	resid = 0.0d0
	! Determine chi2 if using errors. There is a error multiplier that punishes 
	! frequencies of radial order further from 18.
	if (use_error_chi2) then
		do i=1,n
			ww = w(i)
			sf = fun(ww)
			nn = xn(i)
			err_mult = (1/100)*(nn**2) - (9/25)*nn + (106/25)
			resid = resid + ((sd(i)-sf)/(sig(i) * err_mult))**2
		end do
	! or if not using errors
	else if (.not. use_error_chi2) then
		do i=1,n
			ww = w(i)
			sf = fun(ww)
			resid = resid + (sd(i)-sf)**2
		end do
	end if

	fun_val = sngl(1.0 / resid)

end function objfun_ga