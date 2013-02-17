!****************************************************************************
  subroutine poly_smooth (lval,xw,yw,n,xlamb)
! Here, a polynomial fit of degree (N-1), to the N points (XW,YW) is done.
! A normalization of both XW and YW to the interval [0,1] is used to reduce
! numerical errors.
! The construction of the polynomial uses a smoothing parameter XLAMB which
! is associated to the third derivative. The smoothed fit to the N points
! is returned in YW.
!
    use types_and_interfaces, only: dp

    implicit none

    integer, intent(in)     :: lval
    integer, intent(inout)  :: n
    real(dp), intent(in)    :: xlamb
    real(dp), intent(inout), dimension(lval) :: xw, yw

    integer, parameter  :: loc = 100
    real(dp), parameter :: exlim = -200.0_dp
    
    real(dp), dimension(loc)        :: y, c
    real(dp), dimension(loc,loc)    :: d
    
    real(dp)    :: xmax, xmin, ymax, ymin, xa, xb, ya, yb
    real(dp)    :: xj, xk, fac, fac1, fac2, faci1, faci2
    real(dp)    :: sum1
    integer     :: i, j, k
    
    
    ! This assumes that xw and yw are ordered in the correct way
    ! Normalization:
    xmax = xw(n)
    xmin = xw(1)
    ymax = yw(n)
    ymin = yw(1)
    xa = 2.0_dp/(xmax-xmin)
    xb = -(xmax+xmin)/(xmax-xmin)
    ya = 2.0_dp/(ymax-ymin)
    yb = -(ymax+ymin)/(ymax-ymin)

!    do j=1,n
!       xw(j)=xa*xw(j)+xb
!       yw(j)=ya*yw(j)+yb
!    enddo
    xw(1:n) = xa*xw(1:n) + xb
    yw(1:n) = ya*yw(1:n) + yb


    do j=1,n
        xj = dfloat(j)
        do k = j,n
            fac = 0.0d0
            xk = dble(k)
            fac1 = 0.0d0
            fac2 = 0.0d0
            do i = 1,n
                if (xw(i).eq.0.0d0) then
                    faci1 = 0.0d0
                    faci2 = 0.0d0
                    if (k+j.eq.2) faci1 = 1.0d0
                    if (k+j.eq.8) faci2 = 1.0d0
                    goto 240
                endif

                faci1 = xw(i)**(k+j-2)
                faci2 = xw(i)**(k+j-8)

    240         fac1 = fac1+faci1
                fac2 = fac2+faci2
            enddo

            fac = fac2*xlamb*(xk-1.0d0)*(xk-2.0d0)*(xk-3.0d0)
            fac = fac * (xj-1.0d0)*(xj-2.0d0)*(xj-3.0d0)
            fac = fac + fac1

            d(k,j) = fac
            d(j,k) = fac
        enddo

        y(j) = 0.0d0

        ! construct (n-1) degree polynomial
        do i = 1,n
            if (j.eq.1) then    ! constant term
                sum1 = yw(i)
            else
                sum1 = yw(i)*xw(i)**(j-1)
            endif
            y(j) = y(j) + sum1

        enddo
    enddo


    call gauss (loc,d,y,c,n)

    ! de-normalization
    do i = 1,n
        y(i) = c(n)
        do k = n-1,1,-1
            y(i) = y(i)*xw(i) + c(k)
        enddo
        yw(i) = (y(i)-yb)/ya
    enddo

    return
  
  end subroutine poly_smooth
    
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
 
    do k = 1,n-1
        imax = k
        ymax = abs(y(k,k))
        do i = k+1,n
            if (abs(y(i,k)).gt.ymax) then
                imax = i
                ymax = abs(y(i,k))
            endif
        end do
        if (imax.gt.k) then
            do i = k,n
                xx = y(imax,i)
                y(imax,i) = y(k,i)
                y(k,i) = xx
            end do
            xx = f(imax)
            f(imax) = f(k)
            f(k) = xx
        endif
  
        do i = k+1,n
            if (y(i,k).eq.0.0d0) cycle
            b = -y(i,k)/y(k,k)
            do j = k+1,n
                y(i,j) = y(i,j) + b*y(k,j)
            end do
            f(i) = f(i)+b*f(k)
        end do
    end do

    c(n) = f(n)/y(n,n)
    
    do i = n-1,1,-1
        b = 0.d0
        do j = i+1,n
            b = b + y(i,j)*c(j)
        end do
        c(i) = (f(i)-b)/y(i,i)
    end do

    return
    end
!****************************************************************************
