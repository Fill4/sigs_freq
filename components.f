!*******************************************************************************
  real(dp) function bcz_comp (w)
!  this is the signal produced by the sharp transition in the base of the 
!  convection zone
!
!  see Monteiro et al. (1994), eq (20)
!  and Mazumdar et al. (2012), eq (2)

    use types_and_interfaces, only: dp
    use commonarray, only: c
        
    implicit none

    real(dp), intent(in)    :: w
    real(dp)  :: arg, bcz


    ! c(1) = tau_bcz
    ! c(2) = phi_bcz
    ! c(3) = amp_bcz
    ! c(4) = tau_he
    ! c(5) = phi_he
    ! c(6) = amp_he
    ! c(7) = width_he

    arg = 2.0d0 * ( c(1)*w + c(2) )
    bcz  = ( c(3)/w**2 ) * sin(arg)
    
    bcz_comp = bcz

    return
  
  end function bcz_comp
  
  
!*******************************************************************************
  real(dp) function he_comp (w)
!  this is the signal from the helium ionization region
!
!  see Mazumdar et al. (2012), eq (3)

    use types_and_interfaces, only: dp
    use commonarray, only: c
        
    implicit none

    real(dp), intent(in) :: w
    real(dp)  :: arg, he


    ! c(1) = tau_bcz
    ! c(2) = phi_bcz
    ! c(3) = amp_bcz
    ! c(4) = tau_he
    ! c(5) = phi_he
    ! c(6) = amp_he
    ! c(7) = width_he

    arg = 2.0d0 * ( c(4)*w + c(5) )
    he = ( c(6)/w**2 ) * sin(c(7)*w)**2 * cos(arg)
    
    he_comp = he

    return
  
  end function he_comp


