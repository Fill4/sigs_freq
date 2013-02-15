!--------------------------------------------------------------------
	subroutine sec (xlamb,c)
!	 the signal, as given in the function FUN, is subtracted from
!	 the frequencies to obtain the component to be smoothed (the
!	 reduced frequency; Y)
!	 this is then smoothed and used to get the new signal by
!	 subtracting the new smooth component from the frequencies
!	 (which is output in the array SD)
!
!	use gnufor2

		use commonvar, only : nconst, w0ref
		! contains npt, n, l, sd, sig, xn, w -
		use commonarray
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		parameter (nsteps=100,lval=50,ncp=20)
		dimension np(nsteps),x(lval),y(lval),c(ncp)

		common /blockval09/nnp
		common /blockval10/np


		do jn=1,nnp
			ni = np(jn)
			nf = np(jn+1)-1
			
			do i=ni,nf
				ww = w(i)
				ll = l(i)
				x(i+1-ni) = xn(i)
				y(i+1-ni) = w0ref*w(i) - fun(c,ww,ll)
			end do


			m = nf-ni+1

			call poly_smooth (lval,x,y,m,xlamb)

			do i=ni,nf
				sd(i) = w0ref*w(i) - y(i+1-ni)
			end do
			
		end do
   
		return
	
	end subroutine sec
