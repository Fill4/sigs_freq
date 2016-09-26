!--------------------------------------------------------------------
!	Joao Faria: 20/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common variables that all subroutines
!	 need to share. 

module commonvar
	
	use types_and_interfaces, only: dp
	
	implicit none

	! Input variables
	!--------------------------------------------------------
	! Passable Arguments
	logical		:: automatize = .FALSE.
	logical		:: show_plots = .FALSE.
	logical		:: verbose = .FALSE.
	! Range in degree
	integer		:: lmin = 0, lmax = 3
	! Minimum number of modes with same degree
	integer		:: nlmin = 5
	! Range in radial order
	integer		:: nmin = 6, nmax = 30
	! Whether it should use the errors or not
	logical		:: use_error_chi2 = .TRUE.
	! Upper limit for error
	real(dp)	:: ssmax = 0.300
	! Value to work with sigs_diff
	integer		:: degree
	! Fitting control parameter
	real(dp)	:: ftol = 1.0E-04
	! Smoothing control parameter
	real(dp)	:: lambda = -1
	! Fitting procedure values
	integer		:: smooth_iter_max = 3
	! Fitting procedure values
	integer		:: pikaia_pop = 80, pikaia_gen = 3000
	! Reference frequency
	real(dp)	:: w0ref = -1
	! Star parameters
	real		:: large_sep, teff
	! Initial values for parameters
	integer		:: upper_tau_bcz = 4500, lower_tau_bcz = 1500
	integer		:: upper_tau_he2 = 1400, lower_tau_he2 = 300
	!--------------------------------------------------------

	! Other variables used during execution
	!--------------------------------------------------------
	! Time variables to measure code execution time
	real		:: start, finish
	! Number of parameters to fit
	integer		:: nconst  				
	! Constants
	real(dp)	:: pi = 4.0d0*atan(1.0d0)
	!--------------------------------------------------------					

end module commonvar
