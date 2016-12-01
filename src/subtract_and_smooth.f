subroutine subtract_and_smooth (xlamb)
! First, the signal from function fun.f is subtracted from the frequencies to obtain the reduced frequencies.
! In the first iteration no value is removed since c=0.
! Then a n-1 polynomial with third derivative smoothing is fitted to the reduced frequencies and the resulting
! function is then subtrated to the original frequencies to obtain the residuals from the glitches

	use types_and_interfaces, only: dp, fun
	use commonvar, only : w0ref
	use commonarray
	
	implicit none
	 
	real(dp), intent(in) :: xlamb

	integer :: m, ni, nf, i, jn
	integer, parameter :: lval = 50
	real(dp), dimension(lval) :: x, y
	real(dp) :: ww

	
	do jn=1,nnp
		ni = np(jn)
		nf = np(jn+1)-1

		do i=ni,nf
			ww = w(i)
			x(i+1-ni) = xn(i)
			y(i+1-ni) = w0ref*w(i) - fun(ww)
		end do

		m = nf-ni+1

		call poly_smooth (lval,x,y,m,xlamb)

		do i=ni,nf
			sd(i) = w0ref*w(i) - y(i+1-ni)
		end do
	end do

	return

end subroutine subtract_and_smooth
