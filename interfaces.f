! Author        : Joao Faria
! Date          : Oct 2012
! Last changed  : 

module interfaces
!**********************************************************
! This module provides interfaces for some subroutines that
! use assumed-shape arrays as arguments. Each interface is
! a copy of the invocation line and the argument 
! definitions in the subroutine.
! The procedures that call one of this routines:
!       - minimize
!       - ...
! with assumed-shape arrays should include a 
!   'use interfaces' 
! statement.
!**********************************************************
    interface minimize_nl2sol
        subroutine minimize( n, p, x, resid, resd )
	        implicit none
	        integer, intent(in) :: n
	        integer, intent(in) :: p
	        real, intent(inout) :: x(:)
	        real, intent(inout) :: resd
            real, dimension(p)  :: x0
            ! NL2SOL control variables:
	        integer :: uiparm(1)
	        real    :: urparm(1)
	        integer, dimension(60+p)                    :: iv
	        real, dimension( 93+n*(p+3)+p*(3*p+33)/2 )  :: v
		
            external resid
            external ufparm  
        end subroutine minimize
    end interface 
    
end module
