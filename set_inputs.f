!*******************************************************************************
! João Faria: Feb 2013     |     Revised: Filipe Pereira - Abr 2016
!*******************************************************************************
  subroutine set_inputs(options_file)
!   The input options to fit the signal are read here (from a namelist file)

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
								show_plotsd,&
								ssmaxd,&
								lmind,lmaxd	

	! Declaration adn default initialization of all user defined variables
	!Smoothing control parameter
	!real(dp)	:: lambda_initd
	!Fitting control parameter
	real		:: ftold = 0.10E-06
	!Fitting procedure values
	integer		:: smooth_iter_maxd = 5
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
	!Display plots or not
	logical		:: show_plotsd = .FALSE.
	!Upper limit for error
	real		:: ssmaxd = 0.500
	!Range in degree
	integer		:: lmind = 0,lmaxd = 2

	integer :: ierr = 1
	integer :: unit1 = 8

	! Read user defined options file (overwrites default values)
	write (6,*) " Reading the parameters from file: ", options_file
	!Open Options File
	open(unit=unit1, file=options_file, &
					  action='read', delim='quote', &
					  iostat=ierr)

	!Read Options File
	read(unit1, nml=sig_bcz_controls, iostat=ierr)  
	close (unit1)
	if (ierr /= 0) write(*,*) " --> failed in ", trim(options_file), " with error code ", ierr

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
	show_plots = show_plotsd

	ssmax = ssmaxd

	return

  end subroutine set_inputs
