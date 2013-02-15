!--------------------------------------------------------------------
	subroutine fit (c,resd,ierrorflag)
!	 the fit of the function defined in FUNK is done being
!	 c(nconst) the nconst fitted.

		use commonvar, only : pi, nconst,iprint,iterinit,iterfit, &
						  ftolfitlamb => ftol, tolfit, dc0 => dc
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		parameter (ncp=20,ndim=20)
		dimension p(ndim,ndim),y(ndim),c(ncp),c0(ncp)


		ftol = tolfit
		dc = dc0

		! initial guesses for the parameters -
		do i=1,nconst
			c0(i) = c(i)
		end do

		iter=1

		! residuals from initial guesses - 
 1		y(1) = funk(c0)
		
		! build first row of the simplex matrix
		do j=1,nconst
			p(1,j) = c0(j)
		end do

		do i=1,nconst
			do j=1,nconst
				c(j) = c0(j)
			end do
			! update parameters -
			c(i) = c(i) + dc*abs(c0(i))
			if (i.ge.3) then
				if (c0(i).le.1.0d-4) c(i)=1.0d-4
			endif

			y(i+1) = funk(c)
			
			do j=1,nconst
				p(i+1,j) = c(j)
			end do
		end do

		ierrorflag=0
		
		call amoeba (nconst,p,y,ftol,ierrorflag)
		
		
		do i=1,nconst
			c0(i) = 0.0d0
			do j=1,nconst
				c0(i) = c0(i) + p(j,i)
			enddo

			c0(i) = c0(i)/dble(nconst)
		enddo

		if (iter.lt.iterfit) then
			iter = iter+1
			ftol = ftol/10.0d0
			dc = dc/2.0d0
			goto 1
		endif

		do i=1,nconst
			c(i) = abs(c0(i))
		end do

		resd = funk(c)

		if (c(2).gt.1.1d0*pi) c(2) = c(2)-pi
		if (c(2).lt.0.1d0*pi) c(2) = c(2)+pi

	return

	end subroutine fit
