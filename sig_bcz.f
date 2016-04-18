!*******************************************************************************
! João Faria: Feb 2013  |  Revised: Filipe Pereira, 2016
!*******************************************************************************

program sig_bcz_genetic
! This program isolates an oscillatory signal that is present in the oscillation
! frequencies and associated with regions where the sound speed has a discontinuity.
! The general form of the signal is a known function of frequency that depends 
! on a set of parameters C. The differences between the observed frequencies and
! the model ones is minimized in the least squares sense, providing the best fit
! parameters

	use types_and_interfaces, only: dp
	use commonvar
	use commonarray, only: c

	implicit none
	
	integer :: argcount
	character (len=80)   :: afile
	character (len=80)   :: options_file

	real(dp)    :: chi2


	afile='00000'
	write (6,'(/, a, //)')"---------------> PROGRAM SIG_BCZ_genetic <---------------"

!+++++++++++++++++++++++++++++++++++++++++++
!--- process command line argument
!	argcount = iargc()
!	if (argcount /= 1) then
!		write(*,'(a,x,/)') "ERROR! usage: sig_bcz_run options_file"
!		stop
!	endif
!	call getarg(1, options_file)

!--- number of parameters to fit -
	!nconst = 3  ! only bcz
 1	nconst = 7  ! bcz + heII
	allocate(c(nconst))
	options_file = 'options_file'
!--- file with input options -
	call set_inputs(options_file)

!--- initialize all quantities, read in frequencies and create output files -
	call deffreq (afile)
	call init (afile)
	call openfiles (afile)
	call flush (6)

!--- Finding the best parameters -
	call fitlamb (chi2)

!--- Writing the results -
	call output (afile, chi2)

	call flush (9)
	call flush (3)
	close (3)
	write (6,*)"---------------> PROGRAM SIG_BCZ_genetic <---------------"
	call flush (6)
	deallocate(c)

	goto 1

end program sig_bcz_genetic
