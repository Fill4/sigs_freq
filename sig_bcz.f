!----------------------------------------------------------------------------
! 
! João Faria: Feb 2013  |  Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
program sigs_freq
! This program isolates an oscillatory signal that is present in the oscillation
! frequencies and associated with regions where the sound speed has a discontinuity.
!
! The general form of the signal is a known function of frequency that depends 
! on a set of parameters C. 
!
! The differences between the observed frequencies and the model ones is minimized
! in the least squares sense, providing the best fit parameters

	use types_and_interfaces, only: dp
	use commonvar
	use commonarray, only: c
	implicit none
	
	integer				:: narg, iarg
	character (len=80)	:: afile, options_file, star_file
	character (len=20)	:: name
	real(dp)			:: chi2
	real				:: start, finish

	! Check if arguments are found
	narg=command_argument_count()

	if(narg>0)then
	! Loop across options
		do iarg=1,narg
			call get_command_argument(iarg,name)
			select case(adjustl(name))
			case('-h','--help')
				call help
			case('-v','--verbose')
				verbose = .TRUE.
			case('-p','--plots')
				show_plots = .TRUE.
			case default
				write(6,*)"Option ",adjustl(name),"unknown"
			end select
		end do
	end if
	
	call cpu_time(start)

	afile='00000'
	options_file = 'options_file'
	if (verbose) write (6,*)"---------------------> PROGRAM SIGS_FREQ <---------------------"

	! Number of parameters to fit
1	nconst=7
	allocate(c(nconst))

	! Read options_file with input parameters
	call set_inputs(options_file)

	! Initializing all quantities, read in frequencies and create output files
	call deffreq (afile) 	! Reads freqs_list file
	call init (afile) 		! Reads frequencias and calculates 2nd differences
	call openfiles			! Prepare output files

	call flush (6)

	! Finding the best parameters
	call fitlamb (chi2)

	! Writing the results
	call output (afile, chi2)

	call flush (9)
	call flush (3)
	close (3)
	if (verbose) write (6,*)"---------------------> PROGRAM SIGS_FREQ <---------------------"
	call flush (6)
	deallocate(c)

	goto 1

	call cpu_time(finish)
	print '("Time = ",f6.3," seconds.")',finish-start

end program sigs_freq
