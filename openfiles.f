!--------------------------------------------------------------------
! Joao Faria: Jan 2013		|	Revised: Filipe Pereira - Mai 2016
!--------------------------------------------------------------------
subroutine openfiles(afile)
!   open the files necessary for the OUTPUT

	use commonvar
	implicit none
	
	character(len=80), intent(in)   :: afile
	logical :: bool_Results, bool_IterInfo


	! Results file (unit = 9) -
	inquire( file="Results", exist=bool_Results)
	if (bool_Results) then
		open (9, file='Results', status='old', position='append')
	else 
		open (9, file='Results', status='unknown')
		if (verbose) write (9,*) ' '
		close (9)

		open (9,file='Results',status='old')

		! Write header
		if (verbose) then
			write (9,'(x, a, i1, x, a)') "# SIG_GENETIC results (", nconst, "parameters)"
			write (9,*) ''

			write (9,'(x, a, a6, a17, 7a10)') &
				"#", "file", "|", "tau_bcz", "phi_bcz", "amp_bcz", "tau_he", "phi_he", "amp_he", "beta"
			write (9,*) &
				"#---------------------------------------------------------------------------------------------"
			write (9,*) ''
		end if
	endif

	! IterInfo file (unit = 3) -
	inquire( file="Iterinfo", exist=bool_IterInfo)
	if (bool_IterInfo) then
		open (3, file='Iterinfo', status='old', position='append')
		if (verbose) write(3,*) '  File: ', afile
	else 
		open (3, file='Iterinfo', status='unknown')
		if (verbose) write (3,*) ' '
		close (3)

		open (3,file='Iterinfo',status='old')

		! Write header
		if (verbose) then
			write (3,'(x, a, i1, x, a)') "# SIG_GENETIC results (", nconst, "parameters)"
			write (3,*) ''

			write (3,'(a2, a8, a12, a7, a3, 7a10)') &
				"#", "lambda", "iteration", "chi_2", "|", "tau_bcz", "phi_bcz", "amp_bcz", "tau_he", "phi_he", "amp_he", "beta"
			write (3,*) &
				"#------------------------------------------------------------------------------------------------------------"
			write (3,*) ''
			write(3,'(a8, a24, a15, l2)') '  File: ', afile, ' Using Errors: ', use_error_chi2
		end if
	endif


	return

end subroutine openfiles
