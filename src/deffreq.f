subroutine deffreq (afile)
! This subroutine reads all the frequencies files on afile.

	use commonvar
	implicit none
	
	character(len=80), intent(inout)	:: afile
	character(len=80)					:: afile0

	if (afile(1:5).eq.'00000') then
		afile0='freqs'
	else
		afile0='stop'
	endif

	if (use_error_chi2) then
		if (verbose) write (*,'(2x, a)', advance = "no") "name of input file (l,n,v,sigma) --> "
	else if (.not. use_error_chi2) then
		if (verbose) write (*,'(2x, a)', advance = "no") "name of input file (l,n,v) --> "
	endif

	read (*,'(a80)') afile

	if (verbose) write (*,*) ' '

	if (afile(1:1).eq.' ') afile = afile0
	if (afile(1:4).eq.'stop') then
		call cpu_time(finish)
		write(*,*) ' '
		write(*,1024) 'Time elapsed: ', finish-start, 's'
		stop
	endif
	1024 format (2x, a, f10.4, a)

	if (verbose) then
		write (*,*) " Reading frequencies from File: ", afile
		write (*,*) ' '
	end if

	return
end subroutine deffreq
