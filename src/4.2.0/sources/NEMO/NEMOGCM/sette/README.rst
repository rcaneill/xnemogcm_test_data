======================
usage of SETTE package
======================

INSTALLATION
============
* checkout the code as follow:
.. code-block:: console

        $ svn checkout http://forge.ipsl.jussieu.fr/nemo/svn/utils/CI/sette sette

* The sette directory have to be located in the main directory (at the same level as ``src/`` or ``cfg/``).

MASTER SCRIPT: sette.sh 
=======================
* ``sette.sh`` is a simple wrapper that runs tests on the reference configurations, test cases and generates the report at the end.
* ``sette.sh`` is able n option, to take a list of configurations in argument (sette.sh -t "CFG1 CFG2 CFG3"). 
       - The list of available configurations is available in running ./sette_list_avail_cfg.sh.
       - The default for sette is to run all of the reference and test configurations.
* Users need to set up the ``param.cfg`` file correctly and download input files before running the script.

SECONDARY SCRIPT
================
* ``sette_reference-configuration.sh`` runs sette tests on the reference configurations.
* ``sette_test-cases.sh``              runs sette tests on test cases.
* ``sette_list_avail_rev.sh``          generates a listing on all the revisions available in the validation directory for each configuration. 
* ``sette_list_avail_cfg.sh``          generates a listing of all the available configurations.
* ``sette_rpt.sh``                     generates the report.

USER SETUP (recommended)
========================
* in ``param.cfg`` :
      - variables (NEMO_VALIDATION_REF, NEMO_REV_REF) have to be specified in param.cfg
      - variables SETTE_(COMPILER, USING_XIOS, USING_MPMD, JOB_PREFIX_MPMD, JOB_PREFIX_NOMPMD, BATCH_CMD, BATCH_STAT, BATCH_NAME, FORCING_DIR, SVN_CMD, NEMO_VALIDATION_DIR, ADD_NOSIGNEDZERO)
        can be exported from your shell startup files. It is advise to setup the default value in your startup file if it doesn't fit your need.
        If you prefer not doing so, you need to update the default initialisation
      - variable NEMO_VALIDATION_DIR is sette by default in MAIN_DIR/NEMO_VALIDATION (this is to avoid overwritting results when running sette on different branches).
      - description of variables listed in param.cfg (SETTE_):
                
        # reference version for bit reproducibility results
          |  NEMO_VALIDATION_REF : reference directory
          |  NEMO_REV_REF        : reference revision
                
        # compiler information
          |  COMPILER            : compiler among those available in NEMOGCM/ARCH
                
        # XIOS management
          |  USING_XIOS        : flag to control the activation of key_xios. 
                                "yes" to compile using key_xios and link to the external XIOS library.
                                "no"  to compile without key_xios and link to the old IOIPSL library.
          |  USING_MPMD        : flag to control the use of stand-alone IO servers 
                                (requires USING_XIOS="yes").
                                "yes" to run in MPMD (detached) mode with stand-alone IO servers.
                                "no"  to run in SPMD (attached) mode without separate IO servers. 
        # MPI communication management
          |  USING_MPI3        : flag to control the activation of key_mpi3
                                "yes" to use the MPI3 neighbourhood collectives for halo exchange
                                "no" to use standard point-to-point communications for halo exchange
        # loop fusion management
          |  USING_LOOP_FUSION : flag to control the activation of key_loop_fusion
                                "yes" to use the loop fusion adv routines when halo = 2
                                "no" to use standard adv routine

        # generique batch scrip prefix name if MPMD set to true/false
           | JOB_PREFIX_MPMD
           | JOB_PREFIX_NOMPMD
                
        # batch command needed
           | BATCH_CMD           : command for job submission in batch mode
           | BATCH_STAT          : command to check job status
           | BATCH_NAME          : generic sette job name (as it appears with $BATCH_STAT command)
                
        # file storing
           | FORCING_DIR         : directory where is stored input.tar file (same name in input_CONFIG_NAME.cfg)
                                   reference configuration input tar file could be found here : https://gws-access.jasmin.ac.uk/public/nemo/sette_inputs/ 
           | NEMO_VALIDATION_DIR : directory where is stored restarts, run.stat, tracer.stat and ocean.output files for each configuration
                                   ( NEMO_VALIDATION_DIR/WCONFIG_NAME/WCOMPILER_NAME/TEST_NAME/REVISION_NUMBER(or DATE) )
           | INPUT_DIR           : directory where is stored input files (DO NOT CHANGE IT)
                
        # misc.
           | SVN_CMD             : svn command use to do svn info (default svn). Could be useful if you are using git svn
                                 : Reference directory for result comparison will be NEMO_VALIDATION_REF/COMPILER/NEMO_REV_REF
           | ADD_NODIGNEDZERO    : set "yes" if you need key_nosignedzero to run nemo

USAGE of main scripts
=====================
* ``sette.sh``                : it is a simple wrapper to run tests on the reference configurations, test-cases and generate the report at the end.
          - if no argument is given, sette run all the reference configurations and test cases 
          - if a list of argument is provided (sette.sh -t "CFG1 CFG2 CFG3"), sette will only run these configurations. 
             The list of available configurations is available in running ./sette_list_avail_cfg.sh.
          - user need to set up the param.cfg file correctly and download input file before running the script.
          - user can enforce synchronisation (-s option) of the existing CFG_ST with the REF configuration (EXPREF and MY_SRC). 
          - user can enforce cleaning of the CFG_ST configuration (use of makenemo -n CFG_ST clean) (-c option)
* ``sette_rpt.sh``            : it generates the sette report.
          - if no argument is given, the report will be generated on the last changed revision.
          - if an argument is given (revision number) the report will be generated for this revision only
          - if 'old' is given as argument the former behavior is applied (the latest revision is check whatever the current revision
          - it is possible to retreive all the available revision test using sette_list_avail_rev.sh
          - XXXXX+ means sette results for revision XXXXX contain local modification in src/cfgs/test
          - it is NOT possible to run sette_rpt.sh for a single configuration.
* ``sette_list_avail_rev.sh`` : generate a listing on all the revisions available in the validation directory for each configuration.
          - no argument needed
          - XXXXX+ means sette results for revision XXXXX contain local modification in src/cfgs/test
* ``sette_list_avail_cfg.sh`` : generate a listing of all the available configurations.
          - no argument needed

NOTES
=====
* compilation issues:
   - in case of error you can remove your NEW_CONF directory and all files doing :

     ::

     $ ./makenemo -n MY_CONFIG clean_config

   - if you want recompile nemo but before you want to clean compiled code do :

     ::

     ./makenemo clean

   - if you have already compiled you can re-run all sette.sh and compilation part will be by-passed.

* verbose sette output:
   - if you want a completly verbose makenemo you can uncomment `set -x` in makenemo script
     and then launch `./sette.sh 2>&1 | tee out.sette` . This creates out.sette file in ${SETTE_DIR}

TO ADD NEW CONFIGURATION
=========================
1. creates a new ``input_NEW_CONFIG.cfg`` if you need tar file (if you use same tar file of GYRE, ORCA2_LIM or ORCA2_LIM_PISCES you can use it)
2. add a bloc in one of the ``sette_reference-configuration.sh`` or ``sette_test-cases.sh`` script 
3. add your configuration to the list in ``param.cfg``

TO ADD NEW MACHINE
===================
1. add ``arch-compiler.fcm`` in ``NEMOGCM/ARCH`` directory
2. makenemo -m new_compiler  (see makenemo -h for help)
3. add new batch-file

TO ADD NEW BATCH-FILE
======================
1. see in ``SETTE/BATCH_TEMPLATE`` directory existing examples
2. create you own batch file like: ``batch-${COMPILER}`` file
   (note: sette_test-cases.sh and ``sette_cfg-ref.sh`` will copy it in job_batch_template if you run tests in INTERACT_FLAG="no")

EXTRA SETTING POSSIBLE
======================
Among the setting that can be modified by the user in sette_cfg-ref.sh and sette_test-cases.sh script,
there are: sequential/parrallel (default), interacive or not (default) and mpi (default) or not.
  - | BATCH_COMMAND_PAR is the command for job submission in batch mode parallel (specified in param.cfg).
  - | BATCH_COMMAND_SEQ is the command for job submission in batch mode sequential (NB_PROC = 1).
                     the default value is the BATCH_COMMAND_PAR value.
  - | INTERACT_FLAG : "yes" if you want to run in interactive mode.
                    "no"  if you want to run in batch mode (default).     
  - | MPIRUN_FLAG   : "yes" if you want to run in parallel (MPI) (default).
                    "no"  if you want to run in sequential mode (NB_PROC = 1).
