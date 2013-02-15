!**********************************************************
  real(kind=8) function fun2 (c, w, l)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone
!
!	 see Houdek 2007 - eq (17)

        use commonvar, only: w0ref

        implicit none

        real(kind=8), intent(in)  :: c(*)
        ! c(1) argument
        ! c(2) phase
        ! c(3) amplitude
        real(kind=8), intent(in)  :: w
        integer, intent(in)       :: l

        real(kind=8) :: xarg1, xarg2, factor
        real(kind=8) :: w0 = 18.6d0  ! asymptotic mean large frequency separation
        real(kind=8) :: tau0H = 80.0d0   ! value from Houdek 2007


        xarg1 = 2.0d0 * ( c(1)*w + c(2) )
        xarg2 = 2.0d0 * tau0H * w
        factor = 1.0d0 / (dsqrt(1.0d0 + 0.25d0 * tau0H**2 * w**2))

        fun2  = ( c(3) / w**2 ) * factor * dcos(xarg1 + datan(xarg2))

        return
		
  end function fun2

