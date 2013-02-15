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
#FFLAGS = -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fbounds-check -fcheck-array-temporaries -fbacktrace
# normal
FFLAGS = -O0 -ffree-form -Wall -Wtabs -fmax-errors=3 -g
# for production run
#FFLAGS = -Wall -Wextra -Wimplicit-interface -fPIC -Werror -fmax-errors=1 -O3 -march=native -ffast-math -funroll-loops
#dbx     = -O5 -r8 -g
#profil  = -p -O5 -r8 
#samedir = .
#FTN     = ftnchek
LINK = -lmodules -llapack -L/usr/lib


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
JF = /home/joao/Programs/CODE_freqFit/sig_bcz_genetic
JFA = /home/joao/Programs/CODE_freqFit/sig_bcz_genetic/common
JFB = /home/joao/Programs/CODE_freqFit/sig_bcz_genetic/basic
# plot library:
JFp = /home/joao/utils/gnuFor2
# spline library:
JFspl = /home/joao/Programs/CODE_freqFit/dierckx
# miscelaneous library
lib = /home/joao/Programs/fortran/lib

#----------------------------------------------------------

FILES = \
$(JF)/types.o \
$(JF)/commonvar.o \
$(JF)/commonarray.o \
$(JF)/sig_bcz.o \
$(JF)/get_chi_square.o \
$(JF)/deffreq.o \
$(JF)/set_inputs.o \
$(JF)/openfiles.o \
$(JF)/output.o \
$(JF)/writeout.o \
$(JF)/fun.o 
#$(JF)/fun.o


FCOM = \
$(JFA)/init.o \
$(JFA)/fitlamb.o \
$(JFA)/subtract_and_smooth.o


FBASIC = \
$(JFB)/num_to_text.o \
$(JFB)/skpcom.o \
$(JFB)/length.o \
$(JFB)/poly_smooth.o \
$(JFB)/order.o



# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sig_bcz: $(FILES) $(FCOM) $(FBASIC)
	$(FC) $(FILES) $(FCOM) $(FBASIC) -o sig_bcz_run -L$(lib) -I$(lib) $(LINK) -g

clean:
	rm -f $(FILES) $(FCOM) $(FBASIC) *~ *.mod sig_bcz_run
