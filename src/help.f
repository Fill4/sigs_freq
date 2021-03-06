subroutine help

write(*,*) ' '
write(*,*) ' Usage: ./sigs_freq [options] < freqs'
write(*,*) ' '
write(*,*) ' Options:'
write(*,*) '   -h, --help        ', 'Show this help message and exit'
write(*,*) '   -v,--verbose      ', 'Prints information during code execution'
write(*,*) '   -p,--plots        ', 'Shows plots at the end of execution with the results'
write(*,*) '   -a,--auto         ', 'Automatically determines best interval in which to search for final solution.'
write(*,*) '                      Needs teff and large_sep in frequencies file'
write(*,*) ' '
write(*,*) ' Extra parameters can be tweaked in the file options_file'
write(*,*) ' '
write(*,*) ' File freqs has the structure:'
write(*,*) ' '
write(*,*) ' file_with_frequencies'
write(*,*) ' another_file_with_frequencies'
write(*,*) ' stop'
write(*,*) ' '
write(*,*) ' Frequency files have the structure:'
write(*,*) ' '
write(*,*) ' #  l   n     w          error'
write(*,*) '    teff   lerge_sep   (used only if automatic mode is enabled)'
write(*,*) '    0   10    2000.00    0.047'
write(*,*) '    0   12    2235.12    0.053'
write(*,*) '    1   10    2492.32    0.106'
write(*,*) ' '
write(*,*) ' For more information check the repository at: https://github.com/Fill4/sigs_freq'
write(*,*) ' '

stop

end subroutine help