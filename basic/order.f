!--------------------------------------------------------------------
!  Mario Monteiro: May 1995
!  File last changed:
!--------------------------------------------------------------------
	subroutine order (nref,x,m,n,ncols,nrows)
!	 the array X is ordered by increasing X(NREF,*), with the 
!	 parameter N the number of points and M the number of colunms
!	 in XMOD.
!	 WARNING: the dimensions of the arrays are absolute, and so, 
!	 should correspond to the values in the calling subroutines!

		implicit double precision (a-h,o-z)
		implicit integer (i-n)
		parameter (ndim=100)
		dimension x(ncols,nrows),x1(ndim)


		do i=2,n
			in = i-1
			if (x(nref,i).lt.x(nref,in)) then
 15				if (in.ge.2.and.x(nref,i).lt.x(nref,in-1)) then
					in=in-1
					goto 15
				endif
				
				do j=1,m
					x1(j)=x(j,i)
				enddo

				do l=i,in+1,-1
					do j=1,m
						x(j,l)=x(j,l-1)
					enddo
				enddo

				do j=1,m
					x(j,in)=x1(j)
				enddo

			endif
		
		enddo

		return
	
	end subroutine order
