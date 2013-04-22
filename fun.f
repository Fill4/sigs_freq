!**********************************************************
  real(dp) function fun (w)
!  this is the function to be fitted. It corresponds to the the signal
!  produced by the sharp transition in the base of the convection zone
!  added to the one at the helium ionization region
!
!  see Monteiro et al. (1994), eq (20)
!  and Mazumdar et al. (2012), eq (2), (3)

    use types_and_interfaces, only: dp
    use commonarray, only: c
        
    implicit none

    real(dp), intent(in)    :: w
    real(dp)  :: arg, bcz, he


    ! c(1) = tau_bcz
    ! c(2) = phi_bcz
    ! c(3) = amp_bcz
    ! c(4) = tau_he
    ! c(5) = phi_he
    ! c(6) = amp_he
    ! c(7) = width_he

    arg = 2.0d0 * ( c(1)*w + c(2) )
    bcz  = ( c(3)/w**2 ) * sin(arg)

    arg = 2.0d0 * ( c(4)*w + c(5) )
    he = ( c(6)/w ) * (sin(c(7)*w))**2 * cos(arg)
!    he = ( c(6)*w ) * exp(-(c(7)*w)**2) * cos(arg)     ! ** does not work
    
    fun = bcz + he

    return
  
  end function fun

