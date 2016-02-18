!*******************************************************************************
! João Faria: Feb 2013     |     Revised: Filipe Pereira - Jan 2016
!*******************************************************************************
  subroutine set_inputs(options_file)
!   The input options to fit the signal are read here (from a namelist file)

	use commonvar

	implicit none

	character (len=80), intent(in)  :: options_file


	!Defining namelist sig_bcz_controls for easy input of a list of variables
	namelist / sig_bcz_controls / lambda_initd, ftold,&
								lambda_iter_maxd, smooth_iter_maxd,&
								pikaia_popd, pikaia_gend,&
								w0refd,&
								vrightd,vleftd,&
								nlmind,&
								use_error_chi2d,&
								show_plotsd,&
								ssmaxd,&
								lmind,lmaxd	

	!Smoothing control parameter
	real(dp)	:: lambda_initd
	!Fitting control parameter
	real		:: ftold
	!Fitting procedure values
	integer		:: lambda_iter_maxd,smooth_iter_maxd,pikaia_popd, pikaia_gend
	!Reference values -
	real		:: w0refd
	!Borders to ignore in frequency (right and left)
	real		:: vrightd,vleftd
	!Minimum number of modes with same degree
	integer		:: nlmind
	!Whether it should use the errors or not
	logical		:: use_error_chi2d
	!Display plots or not
	logical		:: show_plotsd
	!Upper limit for error
	real		:: ssmaxd
	!Range in degree
	integer		:: lmind,lmaxd

	 
	integer :: ierr = 1
	integer :: unit1 = 8

	!Initialize controls to default values
	include "options_default.dat"

	write (6,*) " Reading the parameters from file: ", options_file
	!Open Options File
	open(unit=unit1, file=options_file, &
					  action='read', delim='quote', &
					  iostat=ierr)

	!Read Options File
	read(unit1, nml=sig_bcz_controls, iostat=ierr)  
	close (unit1)
	if (ierr /= 0) write(*,*) " --> failed in ", trim(options_file), " with error code ", ierr

!    write(6,'( a,/ )') "  Input parameters are:"
!    write(6,nml=sig_bcz_controls)

	!Constants
	pi  = 4.0d0*atan(1.0d0)
	fac = 2.0d-6*pi

	!Attribute input values to all variables
	lambda_init = lambda_initd
	ftol = ftold

	pikaia_gen = pikaia_gend 
	pikaia_pop = pikaia_popd
	lambda_iter_max = lambda_iter_maxd
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
