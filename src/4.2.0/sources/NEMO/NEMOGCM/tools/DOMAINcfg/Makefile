SHELL = /bin/bash
AGRIF=YES

include make.inc

PROGS = make_domain_cfg.exe
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
ifeq ($(AGRIF),YES)
OBJS_AGRIF=obj/agrif2model.o
else
OBJS_AGRIF=
endif	

OBJS_CREATE_DOMCFG=$(addsuffix .o,$(addprefix $(DIR_OBJS)/,$(FILENAMES)))
OBJS_CREATE_DOMCFG := $(filter-out $(OBJS_AGRIF),$(OBJS_CREATE_DOMCFG))

all: depend conv prepagrif AGRIF_LIB $(OBJS_CREATE_DOMCFG) $(PROGS)
		@echo
		@echo ===================================================
		@echo   Nesting is ok
		@echo ===================================================
		@echo


ifeq ($(AGRIF),YES)
make_domain_cfg.exe: $(DIR_LIB)/libdomcfg.a $(DIR_FILES)/nemogcm.F90 $(OBJS_AGRIF) $(OBJS_CREATE_DOMCFG) 
		$(FC) $(LINKFLAGS) $(FFLAGS) -o $(PROGS) $(OBJS_AGRIF) $(DIR_LIB)/libdomcfg.a $(LIBS) -I$(DIR_OBJS) $(FFLAGS_SPEC) -I../../ext/AGRIF -L../../ext/AGRIF -lagrif
else
make_domain_cfg.exe: $(DIR_LIB)/libdomcfg.a $(DIR_FILES)/nemogcm.F90 $(OBJS_CREATE_DOMCFG) 
		$(FC) $(LINKFLAGS) $(FFLAGS) -o $(PROGS)  $(DIR_LIB)/libdomcfg.a $(LIBS) -I$(DIR_OBJS) $(FFLAGS_SPEC)
endif	

ifeq ($(AGRIF),YES)
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
		\cp ../../ext/AGRIF/conv $(DIR_PPSRC)
		\cp ../../ext/AGRIF/agrif_oce.in $(DIR_PPSRC)
		echo $(OBJS_CREATE_DOMCFG)
		@echo

conv:
		$(MAKE) -C ../../ext/AGRIF/LIB

AGRIF_LIB: 
		$(MAKE) -C ../../ext/AGRIF FC=$(FC) CPP=$(CPP) CPPFLAGS="" FFLAGS="$(FFLAGS)"
else

prepagrif:
		@echo
		@echo ===================================================
		@echo   Create working directories  ...
		@echo ===================================================
		mkdir -p obj
		mkdir -p $(DIR_PPSRC_2)
		mkdir -p $(DIR_LIB)
		mkdir -p $(DIR_OBJS)
		@echo
conv:
AGRIF_LIB: 

endif	

$(DIR_LIB)/libdomcfg.a: $(OBJS_CREATE_DOMCFG)
		@echo
		@echo ===================================================
		@echo   Compile domcfg library...
		@echo ===================================================
		$(AR) -r $@ $(OBJS_CREATE_DOMCFG)
		ranlib $@
		@echo



ifeq ($(AGRIF),YES)

$(OBJS_AGRIF):src/agrif2model.F90  $(OBJS_CREATE_DOMCFG) MODEL.CPP 
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS) src/agrif2model.F90 > $(DIR_PPSRC_2)/$(*F).f90	
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  $(FFLAGS_SPEC)

$(DIR_OBJS)/%.o: $(DIR_FILES)/%.F90 make.inc MODEL.CPP 
		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).F90 > $(DIR_PPSRC)/$(*F).f90
		cd NEMOFILES ; ./conv ./agrif_oce.in -rm \
		-comdirin ./ -comdirout AGRIF_MODELFILES/. -convfile $(*F).f90
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS)  $(DIR_PPSRC)/AGRIF_MODELFILES/$(*F).f90 >$(DIR_PPSRC_2)/$(*F).f90	
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  $(FFLAGS_SPEC) 


$(DIR_OBJS)/%.o: $(DIR_FILES)/%.f90 make.inc MODEL.CPP 
		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).f90 > $(DIR_PPSRC)/$(*F).f90
		cd NEMOFILES ; ./conv agrif_oce.in -rm \
		-comdirin ./ -comdirout AGRIF_MODELFILES/. -convfile $(*F).f90
		$(CPP) -I$(DIR_PPSRC)/AGRIF_INC -include MODEL.CPP $(CPPFLAGS)  $(DIR_PPSRC)/AGRIF_MODELFILES/$(*F).f90 >$(DIR_PPSRC_2)/$(*F).f90	
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I../../ext/AGRIF -I$(DIR_OBJS)  $(FFLAGS_SPEC) 

else

$(DIR_OBJS)/%.o: $(DIR_FILES)/%.F90 make.inc MODEL.CPP
		$(CPP) -include MODEL.CPP $(CPPFLAGS) $(DIR_FILES)/$(*F).F90 > $(DIR_PPSRC_2)/$(*F).f90
		$(FC) $(FFLAGS) -c $(DIR_PPSRC_2)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I$(DIR_OBJS)  $(FFLAGS_SPEC) 

$(DIR_OBJS)/%.o: $(DIR_FILES)/%.f90 make.inc MODEL.CPP
		$(FC) $(FFLAGS) -c $(DIR_FILES)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o -I$(DIR_OBJS)  $(FFLAGS_SPEC) 


$(DIR_OBJS)/%.o: $(DIR_FILES)/%.c make.inc MODEL.CPP
	$(CC) -c $(CFLAGS) -c $(DIR_FILES)/$(*F).c -o $(DIR_OBJS)/$(*F).o -I$(DIR_OBJS)  $(FFLAGS_SPEC) 

endif
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
	
# DO NOT DELETE THIS LINE - used by make depend	
