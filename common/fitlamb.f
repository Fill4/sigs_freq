!*******************************************************************************
! Joao Faria: Jan 2013
!*******************************************************************************
  subroutine fitlamb (initial_lambda, resd)
!    this subroutine iterates in LAMBDA from the inital value until it reaches
!    lambda_min or lambda_iter_max iterations are done. For each value of LAMBDA
!    we call the genetic algorithm to find the parameters that minimize the 
!    residuals. 

    use types_and_interfaces
    use commonvar, only : nconst, iprint, lambda_iter_max, lambda, w0ref,fac
    use commonarray, only : c
    
    use lib_pikaia10

    implicit none
    
    real(dp), intent(in)            :: initial_lambda
    real(dp), intent(inout)         :: resd

    character(len=80)               :: outfile


    real    :: ctrl(12), x(nconst), f
    integer :: lambda_iter, seed, status



!    if (iprint.ge.1) call writeout (2,c)

    do lambda_iter = 1, lambda_iter_max

        ! reduce the smoothing parameter by a factor of 2 in each iteration
        lambda = initial_lambda / 2.0d0**(lambda_iter - 1)
        write(*,*) lambda
        
        ! initialize the random-number generator
        seed=13579
        call rninit(seed)
        
        ! set control variables
        ctrl(1:12) = -1
        ctrl(1) = 50
        ctrl(2) = 250
        !ctrl(12) = 2
        outfile = 'param_file'

        ! now call pikaia
        CALL pikaia(objfun_ga, nconst, ctrl, x, f, status)

        ! rescaling parameters
        call rescale(x, c)
        
        
        !     Print the results
        WRITE(*,*) ' status: ', STATUS
        WRITE(*,*) '      x: ', c(1)/(w0ref*fac), c(4)/(w0ref*fac)
        WRITE(*,*) '  chi^2: ', 1./f
        WRITE(*,*) ctrl
            


!!!!** PRINT ******
!!!if (iprint.ge.1) then
!!!call writeout (2,c)
!!!if (iprint.ge.4) call writeout (3,c)
!!!endif
!!!!***************

        if (status /= 0) then
            write(6,*) "Error in PIKAIA"
            stop
        endif


        if (iprint.ge.1) write (3,*) ' '
        
    end do


    if (iprint.ge.3) call writeout (1)

    if (iprint.ge.1) write (6,*) ' '

    resd = 1.0_dp / f

    return

  end subroutine fitlamb
  
  
  
  function objfun_ga(npar, p) result(fun_val)
! This function calculates the objective function value, i.e. the chi^2.
! The signal with the current parameters P is subtracted from the frequencies
! and the result is smoothed by a polynomial before calculating the chi^2
    
    use types_and_interfaces, only: dp, fun, rescale
    use commonvar, only : iprint, lambda, use_error_chi2
    use commonarray
    
    implicit none
    
    integer, intent(in) :: npar ! size of parameter space
    real, intent(in)    :: p(:)
    real                :: fun_val
    
    real(dp)    :: ww, sf, resid
    integer     :: i
    
    ! rescaling parameters
    call rescale(p, c)
    
!    write(*,*) 'hello from objfun'
    ! subtract the signal (given by FUN, with current values of C)
    ! and then smooth again with current XLAMB -
    call subtract_and_smooth(lambda)

    !** PRINT **
    if (iprint.eq.5) then
        call writeout (3)
        write(*,*) c
        write(*,*) 'stoping'
        stop
    endif
    !***********

!    write(*,*) n
    resid = 0.0d0
    ! if not using errors -
    if (use_error_chi2 == 'no' .or. use_error_chi2 == 'n') then
    do i=1,n
        ww = w(i)
        sf = fun(ww)
        resid = resid + (sd(i)-sf)**2
!        write(*,*) sf
    end do
    ! if using errors -
    else if (use_error_chi2 == 'yes' .or. use_error_chi2 == 'y') then
    do i=1,n
        ww = w(i)
        sf = fun(ww)
        resid = resid + ((sd(i)-sf)/sig(i))**2
    end do
    endif

    fun_val = sngl(1.0 / resid)
    
!    write(*,*) 'leaving objfun'
  end function objfun_ga



  subroutine rescale(array_in, array_out)
        
        use types_and_interfaces, only: dp
        use commonvar, only: pi, w0ref, fac
        
        implicit none
        
        real, dimension(:), intent(in)      :: array_in
        real(dp), dimension(:), intent(out) :: array_out

        array_out(1) = dble(array_in(1)) * (4000.*w0ref*fac - 2000.*w0ref*fac) + 2000.*w0ref*fac
        array_out(2) = dble(array_in(2)) * 2.0_dp*pi
        array_out(3) = dble(array_in(3))
        
        array_out(4) = dble(array_in(4)) * (2000.*w0ref*fac - 500.*w0ref*fac) + 500.*w0ref*fac
        array_out(5) = dble(array_in(5)) * 2.0_dp*pi
        array_out(6) = dble(array_in(6))
        array_out(7) = dble(array_in(7)) * (200.*w0ref*fac - 100.*w0ref*fac) + 100.*w0ref*fac
  
  
  end subroutine rescale
