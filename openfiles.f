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
	inquire( file="Results", exist=bool_Results)
	if (bool_Results) then
		open (9, file='Results', status='old', position='append')
	else 
		open (9, file='Results', status='unknown')
		write (9,*) ' '
		close (9)

		open (9,file='Results',status='old')
		! write to terminal that RES was created
		write (6,*) "  In file Results   [filename,C1,C2,...] (all final values)"

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
	inquire( file="IterInfo", exist=bool_IterInfo)
	if (bool_IterInfo) then
		open (3, file='IterInfo', status='old', position='append')
	else 
		open (3, file='IterInfo', status='unknown')
		write (3,*) ' '
		close (3)

		open (3,file='IterInfo',status='old')
		! write to terminal that RES was created
		write (6,*) "  In file IterInfo   [filename,C1,C2,...] (all final values)"

		! header -
		write (3,'(x, a, i1, x, a)') "# SIG_GENETIC results (", nconst, "parameters)"
		write (3,*) ''

		write (3,'(x, a, a6, a17, 7a10)') &
			"#", "file", "|", "tau_bcz", "phi_bcz", "amp_bcz", "tau_he", "phi_he", "amp_he", "beta"
		write (3,*) &
			"#---------------------------------------------------------------------------------------------"
		write (3,*) ''
	endif


	return

  end subroutine openfiles
