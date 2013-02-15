!****************************************************************************
  subroutine poly_smooth (lval,xw,yw,n,xlamb)
! Here, a polynomial fit of degree (N-1), to the N points (XW,YW) is done.
! A normalization of both XW and YW to the interval [0,1] is used to reduce
! numerical errors.
! The construction of the polynomial uses a smoothing parameter XLAMB which
! is associated to the third derivative. The smoothed fit to the N points
! is returned in YW.
!
    implicit double precision (b-h,o-z)
    implicit integer (i-n)
    parameter (loc=100,exlim=-200.0d0)
    dimension xw(lval),yw(lval)
    dimension y(loc),d(loc,loc),c(loc)

    ! This assumes that xw and yw are ordered in the correct way!!!
    ! Normalization:
    xmax=xw(n)
    xmin=xw(1)
    ymax=yw(n)
    ymin=yw(1)
    xa=2.0d0/(xmax-xmin)
    xb=-(xmax+xmin)/(xmax-xmin)
    ya=2.0d0/(ymax-ymin)
    yb=-(ymax+ymin)/(ymax-ymin)

    do j=1,n
       xw(j)=xa*xw(j)+xb
       yw(j)=ya*yw(j)+yb
    enddo


    do j=1,n
	    xj=dfloat(j)
	    do k=j,n
		    fac=0.0d0
		    xk=dble(k)
		    fac1=0.0d0
		    fac2=0.0d0
		    do i=1,n
			    if (xw(i).eq.0.0d0) then
				    faci1=0.0d0
				    faci2=0.0d0
				    if (k+j.eq.2) faci1=1.0d0
				    if (k+j.eq.8) faci2=1.0d0
				    goto 240
    !                else
    !                   xx=abs(xw(i))
    !		    elwx1=dble(k+j-2)*log(xx)
    !		    elwx2=dble(k+j-8)*log(xx)
			    endif
    !		 if (elwx1.lt.exlim) then
    !                   faci1=0.0d0
    !		 else
			    faci1=xw(i)**(k+j-2)
    !		 endif
    !		 if (((j+k).lt.8).or.(elwx2.lt.exlim)) then
    !	            faci2=0.0d0
    !		 else
			    faci2=xw(i)**(k+j-8)
    !                endif
    240			fac1=fac1+faci1
			    fac2=fac2+faci2
		    enddo

		    fac = fac2*xlamb*(xk-1.0d0)*(xk-2.0d0)*(xk-3.0d0)
		    fac = fac * (xj-1.0d0)*(xj-2.0d0)*(xj-3.0d0)

		    fac = fac + fac1
		    d(k,j)=fac
		    d(j,k)=fac
	    enddo

	    y(j)=0.0d0

        ! construct (n-1) degree polynomial
	    do i=1,n
		    if (j.eq.1) then    ! constant term
			    sum=yw(i)
		    else
	     		sum=yw(i)*xw(i)**(j-1)
		    endif
		    y(j)=y(j)+sum
		
	    enddo
    enddo


	call gauss (loc,d,y,c,n)

    ! de-normalization
    do i=1,n
	    y(i)=c(n)
	    do k=n-1,1,-1
		    y(i)=y(i)*xw(i)+c(k)
	    enddo
	    yw(i)=(y(i)-yb)/ya
    enddo

    return
  end
	
!****************************************************************************
	subroutine gauss (lval,y,f,c,n)
! Giving y and f, the system y.c=f is solved being n the dimension:
! y(n,n), c(n) and f(n). It is used the gauss elimination method.
! The Gauss elimination method with partial pivoting is used to solve
! the system.
!
	implicit double precision (b-h,o-z)
	implicit integer (i-n)
	dimension y(lval,lval),f(lval),c(lval)
!
	do 10 k=1,n-1
	   imax=k
	   ymax=abs(y(k,k))
	   do 5 i=k+1,n
	      if (abs(y(i,k)).gt.ymax) then
		 imax=i
		 ymax=abs(y(i,k))
	      endif
 5	   continue
	   if (imax.gt.k) then
	      do 6 i=k,n
		 xx=y(imax,i)
		 y(imax,i)=y(k,i)
		 y(k,i)=xx
 6	      continue
	      xx=f(imax)
	      f(imax)=f(k)
	      f(k)=xx
	   endif
!       
	   do 20 i=k+1,n
	      if (y(i,k).eq.0.0d0) goto 20
	      b=-y(i,k)/y(k,k)
	      do 30 j=k+1,n
		 y(i,j)=y(i,j)+b*y(k,j)
 30	      continue
	      f(i)=f(i)+b*f(k)
 20	   continue
 10	continue
!
	c(n)=f(n)/y(n,n)
	do 40 i=n-1,1,-1
	   b=0.d0
	   do 50 j=i+1,n
	      b=b+y(i,j)*c(j)
 50	   continue
	   c(i)=(f(i)-b)/y(i,i)
 40	continue
!
	return
	end
!****************************************************************************
