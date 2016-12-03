# no console output during compilation:
#.SILENT:

# *********************************************************
# ***					Directories						***
# *********************************************************
BASE := $(abspath ./)

# library
lib := $(abspath ../lib/)

# *********************************************************
# ***					Macros							***
# *********************************************************
FC	= gfortran
FFLAGS = -O2 -ffree-form -ffast-math
LINK = -lmodules

# *******************************************************
# *** Regra que por defeito produz os ficheiros .o **
# *******************************************************
%.o: %.f
	$(FC) $(FFLAGS) -c -o $@ $*.f -I$(lib) $(LINK)
%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $*.f90 -I$(lib) $(LINK)

#----------------------------------------------------------

FILES = \
$(BASE)/types.o \
$(BASE)/commonvar.o \
$(BASE)/commonarray.o \
$(BASE)/deffreq.o \
$(BASE)/read_inputs.o \
$(BASE)/openfiles.o \
$(BASE)/output.o \
$(BASE)/fun.o \
$(BASE)/components.o \
$(BASE)/init.o \
$(BASE)/fitlamb.o \
$(BASE)/subtract_and_smooth.o \
$(BASE)/automatic_interval.o \
$(BASE)/skpcom.o \
$(BASE)/poly_smooth.o \
$(BASE)/help.o \
$(BASE)/main.o
#$(BASE)/rescale.o \

# **********************************************************
# ***             Compilar os programas                  *** 
# **********************************************************

sigs_freq: $(FILES)
	$(FC) $(FILES) -o sigs_freq -L$(lib) -I$(lib) $(LINK) -O2

clean:
	rm -f $(FILES) *~ *.mod sigs_freq
