!--------------------------------------------------------------------
!	Joao Faria: 20/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common variables that all subroutines
!	 need to share. 

module commonvar
	
    use types_and_interfaces, only: dp
	
    implicit none

    logical, public            :: use_error_chi2
    logical, public            :: write_final

    integer, public      :: nconst  ! number of parameters to fit
    integer, public      :: iprint
    integer, public      :: intype  ! type of frequency input
    integer, public      :: lmin, lmax, nlmin   ! lmin - min degree l to consider
                                                ! lmax - max degree l to consider
                                                ! nlmin - min # of l points
    integer, public      :: iterinit,iterfit, lambda_iter_max
    integer, public      :: itermod
    integer, public      :: isel
    integer, public      :: nleft,nright

    real(dp), public    :: fac,pi

    real(dp), public    :: lambda
	
    real(dp), public    :: xinit
    real(dp), public    :: ftol,tolfit,dc
    real(dp), public    :: valtype
    real(dp), public    :: vleft,vright
    real(dp), public    :: xmass,xrad          ! mass & radius of star
    real(dp), public    :: ssmax               ! max error allowed in frequencies
    real(dp), public    :: w0,xl0,xamp0,tau0   ! initial values
    real(dp), public    :: w0ref,xamp0ref,tau0ref,phi0ref  ! reference values
	

end module commonvar
