subroutine set_inputs(options_file)
! This subroutine parses through the options_file assigning the input values to 
! all variables.

	use commonvar
	implicit none

	character (len=80), intent(in)  :: options_file

	!Defining namelist sig_bcz_controls for easy input of a list of variables
	namelist / sig_bcz_controls / lambdad,ftold,&
								smooth_iter_maxd,&
								pikaia_popd, pikaia_gend,&
								w0refd,&
								lmind,lmaxd,&
								nlmind,&
								nmind,nmaxd,&
								vrightd,vleftd,&
								use_error_chi2d,&
								ssmaxd,&
								large_sepd, teffd, lumd,&
								upper_tau_bczd, lower_tau_bczd,&
								upper_tau_he2d,lower_tau_he2d


	! Declaration and default initialization of all user defined variables
	! Smoothing control parameter
	real(dp)	:: lambdad = -1
	! Fitting control parameter
	real		:: ftold = 1.0E-04
	! Fitting procedure values
	integer		:: smooth_iter_maxd = 3
	! Fitting procedure values
	integer		:: pikaia_popd = -1, pikaia_gend = -1
	! Reference frequency
	real		:: w0refd = 2000.0
	! Range in degree
	integer		:: lmind = 0, lmaxd = 2
	! Minimum number of modes with same degree
	integer		:: nlmind = 5
	! Range in radial order
	integer		:: nmind = 0, nmaxd = 100
	! Borders to ignore in frequency (right and left)
	real		:: vrightd = 0.0, vleftd = 0.0
	! Whether it should use the errors or not
	logical		:: use_error_chi2d = .FALSE.
	! Upper limit for error
	real		:: ssmaxd = 0.500
	! Star parameters
	real		:: large_sepd, teffd, lumd
	! Initial values for parameters
	integer		:: upper_tau_bczd = 2500, lower_tau_bczd = 1500
	integer		:: upper_tau_he2d = 1400, lower_tau_he2d = 500

	integer :: ierr = 1
	integer :: unit1 = 8
	character (len=256)            :: message

	! Open user defined options file
	if (verbose) write (6,*) " Reading the parameters from file: ", options_file
	!Open Options File
	open(unit=unit1, file=options_file, &
					  action='read', delim='quote', &
					  iostat=ierr)

	! Read user defined options file (overwrites default values)
	read(unit1, nml=sig_bcz_controls, iostat=ierr, iomsg=message)  
	close (unit1)
	if (ierr /= 0) write(*,*) "Failed reading ", trim(options_file), &
				  " with error code ", ierr, '/', message

	!Constants
	pi  = 4.0d0*atan(1.0d0)

	!Attribute input values to all variables
	lambda = lambdad
	ftol = ftold
	smooth_iter_max = smooth_iter_maxd

	pikaia_gen = pikaia_gend 
	pikaia_pop = pikaia_popd

	w0ref = w0refd

	lmin = lmind
	lmax = lmaxd
	nlmin = nlmind
	nmin = nmind
	nmax = nmaxd
	vleft = vleftd
	vright = vrightd

	use_error_chi2 = use_error_chi2d
	ssmax = ssmaxd

	upper_tau_bcz = upper_tau_bczd
	lower_tau_bcz = lower_tau_bczd
	upper_tau_he2 = upper_tau_he2d
	lower_tau_he2 = lower_tau_he2d

	teff = teffd
	lum = lumd
	large_sep = large_sepd

	return

end subroutine set_inputs
