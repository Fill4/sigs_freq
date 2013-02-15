!--------------------------------------------------------------------
!   Joao Faria: 21/08/2012 
!--------------------------------------------------------------------
!   Module that contains the common arrays that some subroutines
!   need to share. 

module commonarray

    use types_and_interfaces, only: dp

    implicit none

    integer, parameter, public       :: npt = 2000
    integer, public                  :: n

    integer, public, dimension(100) :: np
    integer, public                 :: nnp

    integer, dimension(npt), public  :: l

    real(dp), dimension(npt), public     :: sd
    real(dp), dimension(npt), public     :: sig
    real(dp), dimension(npt), public     :: xn
    real(dp), dimension(npt), public     :: w

    real(dp), allocatable, public   :: c(:) ! parameters of the fit

end module commonarray
