!--------------------------------------------------------------------
  subroutine subtract_and_smooth (xlamb)
!    the signal, as given in the function FUN, is subtracted from
!    the frequencies to obtain the component to be smoothed (the
!    reduced frequency; Y)
!    this is then smoothed and used to get the new signal by
!    subtracting the new smooth component from the frequencies
!    (which is output in the array SD)
!
!   use gnufor2

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
