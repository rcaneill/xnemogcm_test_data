SHELL := /bin/bash
# Necessary otherwise make removes intermediates files
.SECONDARY:

.PHONY: all
all : $(foreach v,3.6 4.0 4.2.0,\
$(foreach f,mesh_mask_1_file mesh_mask_multi_files domcfg_mesh_mask nemo surface_fields open_and_merge nemo_no_grid_in_filename,bld/data/$(v)/$(f))\
)
	echo $^


all/% :\
$(foreach f,mesh_mask_1_file mesh_mask_multi_files domcfg_mesh_mask nemo surface_fields open_and_merge nemo_no_grid_in_filename,bld/data/%/$(f))
	echo $^



# Build container
bld/containers/%/apptainer-nemo.sif : src/%/apptainer-nemo.def src/%/arch_xios src/%/arch_nemo
	mkdir -p $(@D)
	ln -s src/$*/arch_xios .
	ln -s src/$*/arch_nemo .
	apptainer build $@ $<
	rm -rf arch_xios arch_nemo

# Output files that we want to keep
NEMO_OUT := $(foreach point,T U V W,GYRE_1y_00010101_00011230_grid_$(point).nc) \
GYRE_1y_00010101_00011230_surface_grid_T.nc
NEMO_MESH_1 := mesh_mask.nc
NEMO_DOM_1 := domain_cfg_out.nc 
NEMO_DOM_4 := $(foreach proc,0 1 2 3,domain_cfg_out_000$(proc).nc)
NEMO_MESH_4 := $(foreach proc,0 1 2 3,mesh_mask_000$(proc).nc)
NEMO_1 := $(NEMO_OUT) $(NEMO_DOM_1) $(NEMO_MESH_1)
NEMO_4 := $(NEMO_OUT) $(NEMO_DOM_4) $(NEMO_MESH_4)

# Run nemo
$(foreach f,$(NEMO_4),bld/data/%/runs/EXP_4_proc/$(f)) \
$(foreach f,$(NEMO_1),bld/data/%/runs/EXP_1_proc/$(f)) : \
bld/containers/%/apptainer-nemo.sif src/%/input/*
	OUTPUT=bld/data/$*/runs ; \
	mkdir -p $${OUTPUT} ; \
	apptainer run --bind $${OUTPUT}:/nemo/output,src/$*/input:/nemo/input $<
	# clean 4 proc
	cd bld/data/$*/runs/ ; \
	mv EXP_4_proc tmp ; \
	mkdir EXP_4_proc ; \
	cd EXP_4_proc ; \
	for f in $(NEMO_4) ;\
	do  \
	cp ../tmp/$$f . ;\
	done ; \
	rm -r ../tmp
	# clean 1 proc
	cd bld/data/$*/runs/ ; \
	mv EXP_1_proc tmp ; \
	mkdir EXP_1_proc ; \
	cd EXP_1_proc ; \
	for f in $(NEMO_1) ;\
	do  \
	cp ../tmp/$$f . ;\
	done ; \
	rm -r ../tmp

# Sort data
.SECONDEXPANSION:


bld/data/%/mesh_mask_1_file : \
bld/data/%/runs/EXP_1_proc/$(NEMO_MESH_1)
	mkdir -p $@
	ln -sr $^ $@


bld/data/%/mesh_mask_multi_files : \
$(foreach f,$(NEMO_MESH_4),bld/data/%/runs/EXP_4_proc/$(f))
	mkdir -p $@
	ln -sr $^ $@

# Nemo 3.6
bld/data/3.6/domcfg_mesh_mask : \
$(foreach f,$(NEMO_MESH_4),bld/data/3.6/runs/EXP_4_proc/$(f))
	mkdir -p $@
	ln -sr $^ $@

# Nemo >= 4
bld/data/%/domcfg_mesh_mask : \
$(foreach f,$(NEMO_MESH_4),bld/data/%/runs/EXP_4_proc/$(f)) \
$(foreach f,$(NEMO_DOM_4),bld/data/%/runs/EXP_4_proc/$(f))
	mkdir -p $@
	ln -sr $^ $@

bld/data/%/nemo : \
$(foreach point,T U V W,bld/data/%/runs/EXP_1_proc/GYRE_1y_00010101_00011230_grid_$(point).nc)
	mkdir -p $@
	ln -sr $^ $@

bld/data/%/surface_fields : \
$(foreach point,surface_grid_T grid_T,bld/data/%/runs/EXP_1_proc/GYRE_1y_00010101_00011230_$(point).nc)
	mkdir -p $@
	ln -sr $^ $@

bld/data/%/open_and_merge : \
$(foreach point,T U V W,bld/data/%/runs/EXP_1_proc/GYRE_1y_00010101_00011230_grid_$(point).nc) \
bld/data/%/runs/EXP_1_proc/mesh_mask.nc
	mkdir -p $@
	ln -sr $^ $@

bld/data/%/nemo_no_grid_in_filename : \
$(foreach point,T U V W,bld/data/%/runs/EXP_1_proc/GYRE_1y_00010101_00011230_grid_$(point).nc)
	mkdir -p $@
	for f in $^ ; \
	do \
	ln -sr $$f $@/$${f: -4} ; \
	done


