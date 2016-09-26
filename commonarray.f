module commonarray
! Module that has all the array variables that need to be shared between subroutines

	use types_and_interfaces, only: dp

	implicit none

	integer, parameter			:: npt = 500
	integer						:: n

	integer, dimension(10)		:: np
	integer						:: nnp

	integer, dimension(npt)		:: l
	real(dp), dimension(npt)	:: xn
	real(dp), dimension(npt)	:: w
	real(dp), dimension(npt)	:: sig
	real(dp), dimension(npt)	:: sd

	real(dp), allocatable		:: c(:) ! parameters of the fit

end module commonarray
