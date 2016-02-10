!--------------------------------------------------------------------
!	Joao Faria: 20/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common variables that all subroutines
!	 need to share. 

module commonvar
	
	use types_and_interfaces, only: dp
	
	implicit none

	logical, public		:: use_error_chi2
	logical, public		:: show_plots 

	integer, public		:: nconst  ! number of parameters to fit

	integer, public		:: lmin, lmax, nlmin	! lmin - min degree l to consider
												! lmax - max degree l to consider
												! nlmin - min # of l points
	integer, public		:: pikaia_pop, pikaia_gen, lambda_iter_max, smooth_iter_max
	integer, public		:: isel
	integer, public		:: nleft,nright

	real(dp), public	:: fac,pi
	
	real(dp), public	:: lambda_init
	real(dp), public	:: ftol
	real(dp), public	:: vleft,vright
	real(dp), public	:: ssmax                !Max error allowed in frequencies
	real(dp), public	:: w0ref                !Reference values
	

end module commonvar
