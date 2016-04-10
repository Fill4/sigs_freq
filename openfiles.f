!*******************************************************************************
! Joao Faria: Jan 2013
!*******************************************************************************
  subroutine openfiles(afile)
!   open the files necessary for the OUTPUT

	use commonvar

	implicit none
	
	character(len=80), intent(in)   :: afile
	logical :: bool_Results, bool_IterInfo
	integer :: iwrite

	iwrite = 0


	! Results file (unit = 9) -
	inquire( file="Results.dat", exist=bool_Results)
	if (bool_Results) then
		open (9, file='Results.dat', status='old', position='append')
	else 
		open (9, file='Results.dat', status='unknown')
		write (9,*) ' '
		close (9)

		open (9,file='Results.dat',status='old')
		! write to terminal that RES was created
		!write (6,*) "  In file Results   [filename,C1,C2,...] (all final values)"

		! header -
		write (9,'(x, a, i1, x, a)') "# SIG_GENETIC results (", nconst, "parameters)"
		write (9,*) ''

		write (9,'(x, a, a6, a17, 7a10)') &
			"#", "file", "|", "tau_bcz", "phi_bcz", "amp_bcz", "tau_he", "phi_he", "amp_he", "beta"
		write (9,*) &
			"#---------------------------------------------------------------------------------------------"
		write (9,*) ''
	endif

	! Results file (unit = 3) -
	inquire( file="IterInfo.dat", exist=bool_IterInfo)
	if (bool_IterInfo) then
		open (3, file='IterInfo.dat', status='old', position='append')
		write(3,*) '  File: ', afile
	else 
		open (3, file='IterInfo.dat', status='unknown')
		write (3,*) ' '
		close (3)

		open (3,file='IterInfo.dat',status='old')
		! write to terminal that RES was created
		!write (6,*) "  In file IterInfo   [filename,C1,C2,...] (all final values)"

		! header -
		write (3,'(x, a, i1, x, a)') "# SIG_GENETIC results (", nconst, "parameters)"
		write (3,*) ''

		write (3,'(a2, a8, a12, a7, a3, 7a10)') &
			"#", "lambda", "iteration", "chi_2", "|", "tau_bcz", "phi_bcz", "amp_bcz", "tau_he", "phi_he", "amp_he", "beta"
		write (3,*) &
			"#------------------------------------------------------------------------------------------------------------"
		write (3,*) ''
		write(3,'(a8, a24, a15, l2)') '  File: ', afile, ' Using Errors: ', use_error_chi2
	endif


	return

  end subroutine openfiles
