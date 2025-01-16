SHELL = /bin/bash
MPI = NO

include make.inc

PROGS = create_coordinate
SFMAKEDEPEND := ./sfmakedepend
DIR_LIB=lib
DIR_FILES=src
DIR_PPSRC=NEMOFILES
DIR_PPSRC_2=ppsrc
DIR_DOC=doc
DIR_OBJS=obj
DIR_LIB=lib

FILENAMES =$(notdir $(basename $(wildcard src/*.F90))) $(notdir $(basename $(wildcard src/*.F))) $(notdir $(basename $(wildcard src/*.c))) $(notdir $(basename $(wildcard src/*.f))) $(notdir $(basename $(wildcard src/*.f90)))
FILES=src/*
OBJS=$(addsuffix .o,$(addprefix $(DIR_OBJS)/,$(FILENAMES)))
OBJS := $(filter-out obj/agrif2model.o, $(OBJS))

PROG=create_coordinate

all: depend conv prepagrif AGRIF_LIB $(OBJS) $(PROGS)
		@echo
		@echo ===================================================
		@echo   Nesting is ok
		@echo ===================================================
		@echo


create_coordinate: $(DIR_LIB)/libnesting.a $(DIR_PPSRC)/create_coordinate.f90
		$(FC) $(LINKFLAGS) $(FFLAGS) -o create_coordinate -L$(DIR_LIB) -lnesting $(LIBS) -I$(DIR_OBJS) $(FFLAGS_SPEC) -I../../ext/AGRIF  -L../../ext/AGRIF -lagrif

prepagrif:
		@echo
		@echo ===================================================
		@echo   Create working directories for AGRIF ...
		@echo ===================================================
		mkdir -p obj
		mkdir -p $(DIR_PPSRC_2)
		mkdir -p $(DIR_LIB)
		mkdir -p $(DIR_OBJS)
		mkdir -p $(DIR_PPSRC)
		mkdir -p $(DIR_PPSRC)/AGRIF_MODELFILES
		mkdir -p $(DIR_PPSRC)/AGRIF_INC
		\cp ../../ext/AGRIF/LIB/conv $(DIR_PPSRC)
		\cp ../../ext/AGRIF/agrif_oce.in $(DIR_PPSRC)
		@echo

conv:
		$(MAKE) -C ../../ext/AGRIF/LIB

AGRIF_LIB: 
		$(MAKE) -C ../../ext/AGRIF FC=$(FC) CPP=$(CPP) FFLAGS="$(FFLAGS)"

$(DIR_LIB)/libnesting.a: $(OBJS) $(DIR_OBJS)/agrif2model.o
		@echo
		@echo ===================================================
		@echo   Compile Nesting library...
		@echo ===================================================
		$(AR) -r $@ $(OBJS) $(DIR_OBJS)/agrif2model.o
		ranlib $@
		@echo


$(DIR_OBJS)/agrif2model.o: $(DIR_FILES)/agrif2model.F90 Makefile  MODEL.CPP 
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).F90 > $(DIR_PPSRC_2)/$(*F).f90
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  

$(DIR_OBJS)/%.o: $(DIR_FILES)/%.F90 Makefile  MODEL.CPP 
		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).F90 > $(DIR_PPSRC)/$(*F).f90
			@(cd NEMOFILES ; ./conv ./agrif_oce.in -rm \
		-comdirin ./ -comdirout AGRIF_MODELFILES/. -convfile $(*F).f90)
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS)  $(DIR_PPSRC)/AGRIF_MODELFILES/$(*F).f90 >$(DIR_PPSRC_2)/$(*F).f90	
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  -J$(DIR_OBJS)

$(DIR_OBJS)/%.o: $(DIR_FILES)/%.f90 Makefile MODEL.CPP 
		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).f90 > $(DIR_PPSRC)/$(*F).f90
			@(cd NEMOFILES ; ./conv agrif_oce.in -rm \
		-comdirin ./ -comdirout AGRIF_MODELFILES/. -convfile $(*F).f90)
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS)  $(DIR_PPSRC)/AGRIF_MODELFILES/$(*F).f90 >$(DIR_PPSRC_2)/$(*F).f90	
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  -J$(DIR_OBJS) 

#$(DIR_OBJS)/%.o: $(DIR_FILES)/%.F Makefile MODEL.CPP
#		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).F > $(DIR_PPSRC)/$(*F).f
#		$(FC) $(FFLAGS) $(F77FLG) -c $(DIR_PPSRC)/$(*F).f -o $(DIR_OBJS)/$(*F).o

#$(DIR_OBJS)/%.o: $(DIR_FILES)/%.f90 Makefile MODEL.CPP
#		$(FC) $(FFLAGS) -c $(DIR_PPSRC)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o

#$(DIR_OBJS)/par_oce.o: $(DIR_FILES)/par_oce.f90
#		echo $(OBJS) $(DIR_FILES)/

#$(DIR_OBJS)/%.o: $(DIR_FILES)/%.c Makefile MODEL.CPP
#	$(CC) -c $(CFLAGS) -c $(DIR_FILES)/$(*F).c -o $(DIR_OBJS)/$(*F).o

#--------------------------------------------------------------------------
#  Target to create dependeanies.
#--------------------------------------------------------------------------

MakeDepend: $(DIR_FILES)
		$(SFMAKEDEPEND) --silent --fext=f90 --fext=f --fext=F90 --fext=F --fext=c --file=- --moddir=$(DIR_OBJS) --objdir=$(DIR_OBJS)  $(FILES) > MakeDepend

depend: $(DIR_FILES)
		@echo
		@echo ===================================================
		@echo   Create dependancies for Nesting ...
		@echo ===================================================
		$(SFMAKEDEPEND) --fext=f90 --fext=f  --silent --fext=F90 --fext=F --fext=c --file=- --moddir=$(DIR_OBJS) --objdir=$(DIR_OBJS)  $(FILES) > MakeDepend

#ifneq "$(MAKECMDGOALS)" "clean"
-include ./MakeDepend
#endif

#--------------------------------------------------------------------------
#  Target to clean
#--------------------------------------------------------------------------

clean:
		@echo
		@echo ===================================================
		@echo   Clean ...
		@echo ===================================================
		$(RM) -rf $(DIR_PPSRC) $(DIR_PPSRC_2) $(DIR_LIB) $(DIR_OBJS) $(PROGS) 

cleangrif:	
		@echo
		@echo ===================================================
		@echo   Clean agrif and conv ...
		@echo ===================================================
		$(MAKE) clean -C ../../ext/AGRIF/LIB 
		$(MAKE) clean -C ../../ext/AGRIF 

cleanall: clean cleangrif
	
# DO NOT DELETE THIS LINE - used by make depend	
