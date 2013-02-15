! Joao Faria
! Last changed: 19/08/2012 
!--------------------------------------------------------------------
  subroutine deffreq (afile)
!   Define the reference values of the parameters C(NCP)

    use commonvar
    use commonarray, only: c

    character(len=80)    :: afile
    character(len=80)    :: afile0


    tau0 = tau0ref

    ! first 2 parameters
    c(1)=tau0*w0ref*fac ! argument
    write(*,*) c(1), w0ref, fac
    c(2)=phi0ref        ! phase

    ! third parameter
    c(3)=xamp0          ! amplitude

    ! output to terminal -
    !write(*,*) c(1), c(2), c(3)

    if (afile(1:5).eq.'00000') then
        afile0='freqs'
    else
        afile0='stop'
    endif

    write(*,*) ' '

    if (use_error_chi2 == 'yes' .or. use_error_chi2 == 'y') then
        write (*,'(2x, a)', advance = "no") "name of input file (l,n,v,sigma) --> "
    else if (use_error_chi2 == 'no' .or. use_error_chi2 == 'n') then
        write (*,'(2x, a)', advance = "no") "name of input file (l,n,v) --> "
    endif

    read (*,'(a80)') afile

    write (*,*) ' '

    if (afile(1:1).eq.' ') afile = afile0
    if (afile(1:4).eq.'stop') stop

    write (*,*) "  Reading frequencies from File: ", afile

    return
  
  end
