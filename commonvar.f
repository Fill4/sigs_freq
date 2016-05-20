!--------------------------------------------------------------------
!	Joao Faria: 20/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common variables that all subroutines
!	 need to share. 

module commonvar
	
	use types_and_interfaces, only: dp
	
	implicit none

	logical, public		:: use_error_chi2		! Define use of frequency errors
	logical, public		:: show_plots = .FALSE.
	logical, public		:: verbose = .FALSE.	! Toggle print information during execution
	logical, public		:: is_model = .FALSE.

	integer, public		:: nconst  				! Number of parameters to fit
	integer, public		:: lmin, lmax, nlmin	! Min degree l to consider
												! Max degree l to consider
												! Minimum number of l points

	integer, public		:: pikaia_pop			! Controls the initial population size of pikaia
	integer, public		:: pikaia_gen			! Controls the number of generations for pikaia
	integer, public		:: smooth_iter_max		! Number of times the minimization code tries to improve the smooth function
	integer, public		:: isel					! Mostly unused, needs to be reviewed
	integer, public		:: nleft,nright			! 

	real(dp), public	:: fac,pi 				! Constants
	
	real(dp), public	:: ftol					! Stop value for paramter improvement
	real(dp), public	:: vleft,vright			!
	real(dp), public	:: ssmax                ! Max error allowed in frequencies
	real(dp), public	:: w0ref                ! Reference values

	!Variables to use in rescale function
	real(dp), public		:: upper_tau_bcz, lower_tau_bcz
	real(dp), public		:: upper_tau_he2, lower_tau_he2
	real(dp), public		:: upper_beta, lower_beta

	real, public		:: star_mass			
	real, public		:: star_rad
	real, public		:: star_lum				!Various star parameters
	real, public		:: star_teff
	real, public		:: star_age


end module commonvar
