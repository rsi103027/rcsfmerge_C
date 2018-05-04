.SUFFIXES: .f90 .mod

EXE = rcsfmerge_CC
BINDIR = ${GRASP}/bin
GRASPLIB = ${GRASP}/lib
BINFILE = $(BINDIR)/$(EXE)
SRCLIBDIR = ../../lib
MODDIR = ${SRCLIBDIR}/libmod
MODL9290 = ${SRCLIBDIR}/lib9290
F90 = $(FC)
F90_FLAGS = $(FC_FLAGS)
GRASPLIBS =-l9290 -lmod
LAPACK_LIB = -llapack -lblas

APP_LIBS = -L${GRASPLIB} ${GRASPLIBS}

#   Define data types

APP_OBJ= \
	lodcsl_Zero_I.o lodcsl_Part_I.o \
	set_CSF_ZFlist_I.o set_CSF_number_I.o \
\
        RCSFzerofirst.o \
        lodcsl_Zero.o lodcsl_Part.o \
	set_CSF_ZFlist.o set_CSF_number.o

$(EXE): $(APP_OBJ)
	$(F90) -o $(BINFILE) $(APP_OBJ) $(APP_LIBS) \
 		$(APP_LIBS) ${LAPACK_LIB}

.f90.o:
	$(F90) -c $(F90_FLAGS) $< -I $(MODDIR) -I ${MODL9290} -I $(MODDIR) -o $@

.f.o:
	$(FC) -c $(FC_FLAGS) $< -o $@

clean:
	-rm -f *.o core  *.mod
