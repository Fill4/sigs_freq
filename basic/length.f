!--------------------------------------------------------------------
  integer function length (aenter)
!	 Last changed: Aug 2000
!	 the length of the sentence AENTER is determined and returned as LENGTH.

        implicit none
        
        character(len=*) :: aenter
		integer, parameter :: nlet = 150
		integer :: i


		do i=1,nlet-3
			if (aenter(i:i+2).eq.'   ') goto 20
			if (aenter(i:i).eq.'@') goto 20
		enddo

		! no end seems to have been found -
		write (*,*) 'ERROR: Determining the length in LENGTH !'
		stop
		
 20		length=i-1
		if (length.eq.0) length=1

		return
	end function length
