!--------------------------------------------------------------------
	double precision function funk(c)
!	 the function to be minimized is defined as the residuals for
!	 a fit in a least squares sense

		use commonvar, only : nconst, use_error_chi2
		! contains npt, n, l, sd, sig, xn, w -
		use commonarray
		
	 	implicit double precision (b-h,o-z)
		implicit integer (i-n)
		parameter (ncp=20)
		dimension c(ncp)

		
		resid = 0.0d0
		! if not using errors -
		if (use_error_chi2 == 'no' .or. use_error_chi2 == 'n') then
			do i=1,n
				ww = w(i)
				ll = l(i)
				sf = fun(c,ww,ll)
				resid = resid + (sd(i)-sf)**2
			end do
		! if using errors -
		else if (use_error_chi2 == 'yes' .or. use_error_chi2 == 'y') then
			do i=1,n
				ww = w(i)
				ll = l(i)
				sf = fun(c,ww,ll)
				resid = resid + ((sd(i)-sf)/sig(i))**2
			end do

		endif
        
		funk = resid

		return
	
	end function funk
