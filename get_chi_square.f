!**********************************************************
! 
!
!
!**********************************************************
  subroutine get_chi_square (chi2, chi2norm)
   
    ! contains npt, n, l, sd, sig, xn, w -
    use commonarray
    use commonvar
    
    implicit double precision (b-h,o-z)
    
    real(dp), intent(inout)         :: chi2, chi2norm
    
    real(dp)    :: chi2term, chi2sum
    real(dp)    :: fit, ww
    real(dp)    :: signal_err
    integer     :: i, chi2N
    integer     :: ll

    
    chi2sum = 0.0
    chi2N = 0
    
    do i=1,n
        ! the smooth component must be removed from the sum of
        ! frequency+error in each point
        ww = w(i)
        ll = l(i)
        signal_err = (w0ref*ww + sig(i)) - (w0ref*ww - sd(i))
        fit = fun(ww)
        chi2term = ((sd(i) - fit) / signal_err)**2
        chi2sum = chi2sum + chi2term
        chi2N = chi2N + 1
    enddo

    chi2 = chi2sum
    ! normalized chi^2:
    chi2norm = chi2sum / (chi2N - nconst)

    return

  end subroutine
