!----------------------------------------------------------------------------
! João Faria: Feb 2013     |     Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
  subroutine set_inputs(options_file)
! This subroutine parses through the options_file assigning the input values to 
! all variables.

	use commonvar
	implicit none

	character (len=80), intent(in)  :: options_file

	!Defining namelist sig_bcz_controls for easy input of a list of variables
	namelist / sig_bcz_controls / ftold,&
								smooth_iter_maxd,&
								pikaia_popd, pikaia_gend,&
								w0refd,&
								vrightd,vleftd,&
								nlmind,&
								use_error_chi2d,&
								ssmaxd,&
								lmind,lmaxd,&
								upper_tau_bczd, lower_tau_bczd,&
								upper_tau_he2d,lower_tau_he2d


	! Declaration adn default initialization of all user defined variables
	!Smoothing control parameter
	!real(dp)	:: lambda_initd
	!Fitting control parameter
	real		:: ftold = 0.10E-06
	!Fitting procedure values
	integer		:: smooth_iter_maxd = 4
	!Fitting procedure values
	integer		:: pikaia_popd = -1
	!Fitting procedure values
	integer		:: pikaia_gend = -1
	!Reference values -
	real		:: w0refd = 2000.0
	!Borders to ignore in frequency (right and left)
	real		:: vrightd = 0.0, vleftd = 0.0
	!Minimum number of modes with same degree
	integer		:: nlmind = 5
	!Whether it should use the errors or not
	logical		:: use_error_chi2d = .FALSE.
	!Upper limit for error
	real		:: ssmaxd = 0.500
	!Range in degree
	integer		:: lmind = 0,lmaxd = 2
	!Initial values for parameters
	integer		:: upper_tau_bczd, lower_tau_bczd
	integer		:: upper_tau_he2d, lower_tau_he2d 

	integer :: ierr = 1
	integer :: unit1 = 8
	character (len=256)            :: message

	! Read user defined options file (overwrites default values)
	if (verbose) write (6,*) " Reading the parameters from file: ", options_file
	!Open Options File
	open(unit=unit1, file=options_file, &
					  action='read', delim='quote', &
					  iostat=ierr)

	!Read Options File
	read(unit1, nml=sig_bcz_controls, iostat=ierr, iomsg=message)  
	close (unit1)
	if (ierr /= 0) write(*,*) " Failed reading ", trim(options_file), &
				  " with error code ", ierr, '/', message

	!Constants
	pi  = 4.0d0*atan(1.0d0)
	fac = 2.0d-6*pi

	!Attribute input values to all variables
	!lambda_init = lambda_initd
	ftol = ftold

	pikaia_gen = pikaia_gend 
	pikaia_pop = pikaia_popd
	smooth_iter_max = smooth_iter_maxd

	w0ref = w0refd

	lmin = lmind
	lmax = lmaxd
	nlmin = nlmind
	isel = 0

	use_error_chi2 = use_error_chi2d
	ssmax = ssmaxd

	upper_tau_bcz = upper_tau_bczd
	lower_tau_bcz = lower_tau_bczd
	upper_tau_he2 = upper_tau_he2d
	lower_tau_he2 = lower_tau_he2d

	return

  end subroutine set_inputs
