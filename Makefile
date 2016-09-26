# no console output during compilation:
#.SILENT:

# *******************************************************
# ***          Comecar por limpar os sufixos          ***
# *******************************************************
.SUFFIXES:


# *******************************************************
# ***                       Macros                    ***
# *******************************************************
FC	= gfortran
# for development
#FFLAGS = -ffree-form -Wall -Wtabs -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fbounds-check -fcheck-array-temporaries -fbacktrace
FFLAGS = -O3 -ffree-form -ffast-math -fexternal-blas
#FFLAGS = -O3 -ffree-form
# normal

# for production run
#FFLAGS = -ffree-form -fPIC -fmax-errors=1 -O3 -march=native -ffast-math -funroll-loops
#dbx     = -O5 -r8 -g
#profil  = -p -O5 -r8 
#samedir = .
#FTN     = ftnchek
LINK = -lmodules -llapack -lblas -L/usr/lib

# *******************************************************
# *** Regra que por defeito produz os ficheiros .o **
# *******************************************************
%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $*.f -I$(lib) $(LINK) 
%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $*.f90 -I$(lib) $(LINK) 


# *******************************************************
# ***   Especificar as directorias com as subrotinas  ***
# *******************************************************

BASE := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# library
lib := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))../lib-fortran)

#----------------------------------------------------------

FILES = \
$(BASE)/types.o \
$(BASE)/commonvar.o \
$(BASE)/commonarray.o \
$(BASE)/deffreq.o \
$(BASE)/set_inputs.o \
$(BASE)/openfiles.o \
$(BASE)/output.o \
$(BASE)/fun.o \
$(BASE)/components.o \
$(BASE)/init.o \
$(BASE)/fitlamb.o \
$(BASE)/subtract_and_smooth.o\
$(BASE)/skpcom.o \
$(BASE)/poly_smooth.o \
$(BASE)/help.o \
$(BASE)/sig_bcz.o
#$(BASE)/rescale.o \

# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sigs_freq: $(FILES)
	$(FC) $(FILES) -o sigs_freq -L$(lib) -I$(lib) $(LINK) -O3

clean:
	rm -f $(FILES) *~ *.mod sigs_freq
