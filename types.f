!-------------------------------------------------------------------------------
!	Joao Faria: Feb 2013
!       Module that contains common types and interfaces used by subroutines
!-------------------------------------------------------------------------------

module types_and_interfaces

    implicit none
    
    public
    
    integer,parameter :: sp = selected_real_kind(p=6,r=37)
    integer,parameter :: dp = selected_real_kind(p=15,r=307)

    ! provide interface to the objective function (genetic algorthim type)
    interface
      function objfun_ga(n, p) result(fun_val)
        implicit none
        integer, intent(in)    :: n
        real, intent(in)       :: p(:)
        real                   :: fun_val
      end function objfun_ga
    end interface

   ! interface to the function that calculates the signal
    interface fun
        real(kind=8) function fun (w)
            implicit none
            real(kind=8), intent(in)  :: w
        end function fun
    end interface fun
    
    
    ! interface to the parameter rescaling subroutine
    interface rescale
      subroutine rescale(array_in, array_out)
        implicit none
        real, dimension(:), intent(in)      :: array_in
        real(kind=8), dimension(:), intent(out) :: array_out
      end subroutine rescale
    end interface rescale
    
    
end module types_and_interfaces