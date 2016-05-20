# no console output during compilation:
#.SILENT:

# *******************************************************
# ***          Comecar por limpar os sufixos          ***
# *******************************************************
.SUFFIXES:


# *******************************************************
# ***                       Macros                    ***
# *******************************************************
#FC = /usr/bin/f77
#FC	= g77 -Wall
FC	= gfortran
# for development
#FFLAGS = -ffree-form -Wall -Wtabs -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fbounds-check -fcheck-array-temporaries -fbacktrace
FFLAGS = -O3 -ffree-form -ffast-math -fexternal-blas
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
BASE = /home/fill/Documents/repos/glitch

# library
lib = /home/fill/Documents/repos/lib-fortran

#----------------------------------------------------------

FILES = \
$(BASE)/types.o \
$(BASE)/commonvar.o \
$(BASE)/commonarray.o \
$(BASE)/get_chi_square.o \
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
$(BASE)/rescale.o \
$(BASE)/help.o \
$(BASE)/star_par.o \
$(BASE)/sig_bcz.o

# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sig_bcz: $(FILES)
	$(FC) $(FILES) -o sig_bcz_run -L$(lib) -I$(lib) $(LINK) -O3

clean:
	rm -f $(FILES) *~ *.mod sig_bcz_run
