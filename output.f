!*******************************************************************************
! João Faria: Feb 2013
!*******************************************************************************
  subroutine output (afile, amess)
!   write the OUTPUT

    use types_and_interfaces, only: dp, fun
    use commonvar
    use commonarray, only: c, w, sd, sig, n

    use lib_array
    use lib_plot

    implicit none

    character(len=80) :: afile
    character(len=1)  :: amess

    real(dp)            :: taud
    real(dp)            :: chi2, chi2norm

    real(dp), dimension(150) :: xx, resultfun
    real(dp)                :: min_xx, max_xx

    integer :: nfile, length, i


    taud = c(1) / (w0ref*fac)
    call get_chi_square(chi2, chi2norm)

    write (6,*) "  Frequencies from file: ", afile
    
    write(*,*) taud, c(1)
    
    write (6,1010) 'Results:', &
                        'Tau_d = ', taud, 'Phi_d = ', c(2), &
                        'A_d = ', c(3), &
                        'chi2 = ', chi2, 'chi2norm = ', chi2norm

 1010   format (3x, a, //, &
                6x, a, f9.4, 6x, a, f8.5, //, &
                6x, a, f10.4, //, &
                6x, a, f15.3, 5x, a, f15.3, /)

    ! ploting the results
    !   create array with smooth function
    min_xx = minval(w(1:n))! - 1.0d-4
    max_xx = maxval(w(1:n))! + 1.0d-4
!    write(*,*) min_xx*w0ref, max_xx*w0ref
!    write(*,*) n
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
        write (9,9003) afile(nfile-6:nfile-4), taud, c(2), c(3), amess
 9003   format (3x, a, x, f9.4, 2x, f7.5, 2x, f10.8, 2x, a1)
    endif

        return
  
  end subroutine output
