!*******************************************************************************
! João Faria: Feb 2013
!*******************************************************************************
  subroutine output (afile, amess, chi2)
!   write the OUTPUT

    use types_and_interfaces, only: dp, fun
    use commonvar
    use commonarray, only: c, w, sd, sig, n

    use lib_array
    use lib_plot

    implicit none

    character(len=80) :: afile
    character(len=1)  :: amess

    real(dp), intent(in)  :: chi2
    real(dp)              :: chi2norm
    real(dp)              :: tau_bcz, tau_he, beta
    
    real(dp), dimension(150)  :: xx, resultfun
    real(dp)                  :: min_xx, max_xx

    integer :: nfile, length, i

    ! parameters that are in seconds need to be converted
    tau_bcz = c(1) / (w0ref*fac)
    tau_he = c(4) / (w0ref*fac)
    beta = c(7) / (w0ref*fac)
    
    chi2norm = chi2 / (n-nconst)

    write (6,*) "  Frequencies from file: ", afile
    
    write (6,1010) 'Results:', &
                        'tau_BCZ = ', tau_bcz, 'Phi = ', c(2), &
                        'A_BCZ = ', c(3), &
                        'tau_HeII = ', tau_he, 'Phi = ', c(5), &
                        'A_HeII = ', c(6), 'beta_HeII = ', beta, &
                        'chi2 = ', chi2, 'chi2norm = ', chi2norm

 1010   format (3x, a, //, &
                6x, a11, f10.2, 6x, a, f8.5, //, &
                6x, a11, f10.4, //, &
                6x, a11, f10.2, 6x, a, f8.5, //, &
                6x, a11, f10.4, 6x, a, f8.2, //, &
                //, &
                6x, a, f15.3, 5x, a, f15.3, /)


    ! ploting the results
    !   create array with smooth function
    min_xx = minval(w(1:n))! - 1.0d-4
    max_xx = maxval(w(1:n))! + 1.0d-4
    call linspace(min_xx, max_xx, xx)
    do i=1,150
        resultfun(i) = fun(xx(i))
    end do

    call plot(dble(w(1:n)*w0ref), dble(sd(1:n)), xx*w0ref, resultfun, &
                  ' 5.00-',color2='black',color1='green', &
                  errors=dble(sig(1:n)) )!, &
                  !terminal='png')
                  !yrange=(/-3.0d0,3.0d0/) )
                  
                  
                  
    nfile = length(afile)

    ! output to "res" file -
    if (intype.eq.0) then
        write (9,9003) afile(nfile-6:nfile-4), tau_bcz, c(2), c(3), amess
 9003   format (3x, a, x, f9.4, 2x, f7.5, 2x, f10.8, 2x, a1)
    endif

        return
  
  end subroutine output
