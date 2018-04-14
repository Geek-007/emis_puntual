#
#	Lower level Makefile for pemas.exe  program
#       Suffix rules and commands
#######################
FIX01 =
#######################

.IGNORE:

.SUFFIXES:      .f90 .f .i .o

.f90.o:
		$(RM) $@ 
		$(CPP) $(CPPFLAGS) -D$(MACH) $(FIX01) $*.f90 > $*.f
		$(FC) -c $(FCFLAGS) $*.f
		

.f90.f:
		$(RM) $@
		$(CPP) $(CPPFLAGS) -D$(MACH) $(FIX01) $*.f90 > $*.f

.f.o:
		$(RM) $@
		$(FC) -c $(FCFLAGS) $(FIX01) $*.f
		

OBJS	=	vars_dat_mod.o reads_input.o pemas.o salidas.o computations.o \


SRC	=	$(OBJS:.o=.f)


cray dec hp ibm sgi sun default:
		@echo "you need to be up a directory to make pemas.exe"

all:: 		pemas.exe

pemas.exe:	$(OBJS)
		$(FC) -o $@ $(LDOPTIONS) $(OBJS) $(LOCAL_LIBRARIES)

code:		$(SRC2) 

reads_input.o pemas.o salidas.o computations.o : vars_dat_mod.o

# -------------------------------------------------------------------------
# DO NOT DELETE THIS LINE -- make depend depends on it.

clean:
		$(RM) $(RM_LIST)
