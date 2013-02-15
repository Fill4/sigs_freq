!**********************************************************
  real(dp) function fun (w)
!  this is the function to be fitted. It is the signal
!  produced by the sharp transition in the base of the
!  convection zone
!
!  see Monteiro et al. (1994), eq (20)

    use types_and_interfaces, only: dp
    use commonarray, only: c
        
    implicit none

    real(dp), intent(in)    :: w
    real(dp)  :: xarg

    xarg = 2.0d0 * ( c(1)*w + c(2) )
    fun  = ( c(3)/w**2 ) * sin(xarg)

    return
  
  end function fun

