!*******************************************************************************
! Joao Faria: Jan 2013
!*******************************************************************************
  subroutine openfiles(afile)
!   open the files necessary for the OUTPUT

    use commonvar

    implicit none
    
    character(len=80), intent(in)   :: afile
    integer :: iwrite

    iwrite = 0


    ! RES file (unit = 9) -
    !inquire( file="/home/joao/Documents/FCUP/TESE/bcz_fit/res", exist=res_exists)

    if (iwrite.eq.0) then
        iwrite=1
        open (9, file='res', status='unknown')
        write (9,*) ' '
        close (9)
        open (9,file='res',status='old')
        ! write to terminal that RES was created
        write (6,*) "  File RES   [Model,C1,C2,...] (all final values)"

        ! header -
        write (9,9001) "# SIG_BCZ results (", nconst, "parameters)"
 9001   format (x, a, i1, x, a, /) 
           
        write (9,9002) "# Frequency data from: ", afile
 9002   format (x, a, x, a20, /)


        if (intype.eq.0) then
              write (9,*) "# Mod  Tau_d       Phi      A_d"
              write (9,*) "#------------------------------------"
              
     
           else if (intype.eq.1) then
              write (9,1065)
 1065         format('#',/,'# Mod  Tau_d       Phi  ',&
                  '    A_d           cldovs',/,&
                  '#----------------------------------------',&
                  '-------------------------')
           else if (intype.eq.2) then
              write (9,1066)
 1066         format('#',/,'# M   Tau_d     Phi  ',&
                  '    A_d      mass   w_ref   fw',/,&
                  '#--------------------------------------------',&
                  '-------------------------')
        else
              write (*,*) 'WARNING: No output to COF!'
           endif
    endif

    ! COF file (unit = 3) -
    if (iprint.ge.1 .and. iprint.le.4) then
        close (3)
        open (3, file='cof', status='unknown')
        write (3,*) ' '
        close (3)
        open (3, file='cof', status='unknown')
        ! write to terminal that COF was created
        write (6,*) "  File COF   [C1,C2,...] (each iteration)"

        write (3,*) "# tau_d     phi        A_d   "
        write (3,*) '#-----------------------------------'
        call flush (3)
    endif

    ! SIG file (unit = 10) -
    if (iprint.ge.3.and.iprint.le.4) then
        close (10)
        open (10, file='sig', status='unknown')
        write (10,*) ' '
        close (10)
        open (10, file='sig', status='unknown')
        ! write to terminal that SIG was created
        write (6,*) "  File SIG   [v, v-smooth, l, n, sigma, fit] "

        write (10,*) "# Final signal isolated by SIG_BCZ!"
        write (10,'(x,a,a20)') "# in frequencies from: ", afile
    endif


    ! QFT file (unit = 7) -
    if (iprint.eq.5) then
        close (7)
        open (7, file='qft', status='unknown')
        write (7,*) ' '
        close (7)
        open (7, file='qft', status='unknown')
        ! write to terminal that QFT was created
        write (6,*) "  File QFT   [v, v-smooth, l, n, sigma, fit] (1st iteration only) "

        write (7,*) "# Signal isolated in the 1st iteration"
        write (7,'(x,a,a20)') "# in frequencies from: ", afile
    endif


    return

  end subroutine openfiles
