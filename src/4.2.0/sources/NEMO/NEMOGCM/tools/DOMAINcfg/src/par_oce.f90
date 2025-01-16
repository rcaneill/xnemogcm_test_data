MODULE par_oce
   !!======================================================================
   !!                        ***  par_oce  ***
   !! Ocean :   set the ocean parameters
   !!======================================================================
   !! History :  OPA  !  1991     (Imbard, Levy, Madec)  Original code
   !!   NEMO     1.0  !  2004-01  (G. Madec, J.-M. Molines)  Free form and module
   !!            3.3  !  2010-09  (C. Ethe) TRA-TRC merge: add jpts, jp_tem & jp_sal
   !!----------------------------------------------------------------------
   USE par_kind          ! kind parameters

   IMPLICIT NONE
   PUBLIC

   INTEGER , PUBLIC                                      ::   nn_dttrc      !: frequency of step on passive tracers
   CHARACTER(lc) ::   cp_cfg           !: name of the configuration
   CHARACTER(lc) ::   cp_cfz           !: name of the zoom of configuration
   INTEGER       ::   jp_cfg           !: resolution of the configuration

   ! data size                                       !!! * size of all input files *
   INTEGER       ::   jpidta           !: 1st lateral dimension ( >= jpi )
   INTEGER       ::   jpjdta           !: 2nd    "         "    ( >= jpj )
   INTEGER       ::   jpkdta           !: number of levels      ( >= jpk )
   LOGICAL       ::   ln_e3_dep        ! e3. definition flag
   LOGICAL       ::   ln_dept_mid      !: set cell depths at cell centers 
   REAL(wp)      ::   pp_not_used       = 999999._wp   !: vertical grid parameter
   REAL(wp)      ::   pp_to_be_computed = 999999._wp   !:    -      -       -
   !!----------------------------------------------------------------------
   !!                   namcfg namelist parameters
   !!----------------------------------------------------------------------
   LOGICAL       ::   ln_read_cfg=.FALSE.    !: (=T) read the domain configuration file or (=F) not
   CHARACTER(lc) ::      cn_domcfg        !: filename the configuration file to be read
   LOGICAL       ::   ln_write_cfg     !: (=T) create the domain configuration file
   CHARACTER(lc) ::      cn_domcfg_out    !: filename the configuration file to be read
   !
   LOGICAL       ::   ln_use_jattr     !: input file read offset
   !                                   !  Use file global attribute: open_ocean_jstart to determine start j-row 
   !                                   !  when reading input from those netcdf files that have the 
   !                                   !  attribute defined. This is designed to enable input files associated 
   !                                   !  with the extended grids used in the under ice shelf configurations to 
   !                                   !  be used without redundant rows when the ice shelves are not in use.
   ! 

   !!---------------------------------------------------------------------
   !! Domain Matrix size 
   !!---------------------------------------------------------------------

   ! time dimension
   INTEGER, PUBLIC, PARAMETER :: jpt = 3    !: time dimension

   ! global domain size               !!! * total computational domain *
   INTEGER       ::   jpiglo           !: 1st dimension of global domain --> i-direction
   INTEGER       ::   jpjglo           !: 2nd    -                  -    --> j-direction
   INTEGER       ::   jpkglo           !: 3nd    -                  -    --> k levels

   ! global domain size for AGRIF     !!! * total AGRIF computational domain *
   INTEGER, PUBLIC            ::   nbug_in_agrif_conv_do_not_remove_or_modify = 1 - 1
   INTEGER, PUBLIC, PARAMETER ::   nbghostcells = 4 !: number of ghost cells: default value
   INTEGER, PUBLIC            ::   nbghostcells_x_w   !: number of ghost cells in i-direction (west)
   INTEGER, PUBLIC            ::   nbghostcells_x_e   !: number of ghost cells in i-direction (east)
   INTEGER, PUBLIC            ::   nbghostcells_y_s   !: number of ghost cells in j-direction (south)
   INTEGER, PUBLIC            ::   nbghostcells_y_n   !: number of ghost cells in j-direction (north)
   INTEGER, PUBLIC            ::   nbcellsx   ! = Ni0glo - nbghostcells_x_w - nbghostcells_x_e  !: number of cells in i-direction
   INTEGER, PUBLIC            ::   nbcellsy   ! = Nj0glo - nbghostcells_y_s - nbghostcells_y_n  !: number of cells in j-direction

   ! local domain size                !!! * local computational domain *
   INTEGER, PUBLIC ::   jpi   !                                                    !: first  dimension
   INTEGER, PUBLIC ::   jpj   !                                                    !: second dimension
   INTEGER, PUBLIC ::   jpk   ! = jpkglo                                           !: third  dimension
   INTEGER, PUBLIC ::   jpim1 ! = jpi-1                                            !: inner domain indices
   INTEGER, PUBLIC ::   jpjm1 ! = jpj-1                                            !:   -     -      -
   INTEGER, PUBLIC ::   jpkm1 ! = jpk-1                                            !:   -     -      -
   INTEGER, PUBLIC ::   jpij  ! = jpi*jpj                                          !:  jpi x jpj
   INTEGER, PUBLIC ::   jpimax! = ( jpiglo-2*nn_hls + (jpni-1) ) / jpni + 2*nn_hls !: maximum jpi
   INTEGER, PUBLIC ::   jpjmax! = ( jpjglo-2*nn_hls + (jpnj-1) ) / jpnj + 2*nn_hls !: maximum jpj

   !!---------------------------------------------------------------------
   !! Active tracer parameters
   !!---------------------------------------------------------------------
   INTEGER, PUBLIC, PARAMETER ::   jpts   = 2    !: Number of active tracers (=2, i.e. T & S )
   INTEGER, PUBLIC, PARAMETER ::   jp_tem = 1    !: indice for temperature
   INTEGER, PUBLIC, PARAMETER ::   jp_sal = 2    !: indice for salinity

   !!----------------------------------------------------------------------
   !!   Domain decomposition
   !!----------------------------------------------------------------------
   !! if we dont use massively parallel computer (parameters jpni=jpnj=1) so jpiglo=jpi and jpjglo=jpj
   INTEGER, PUBLIC            ::   jpni         !: number of processors following i 
   INTEGER, PUBLIC            ::   jpnj         !: number of processors following j
   INTEGER, PUBLIC            ::   jpnij        !: nb of local domain = nb of processors ( <= jpni x jpnj )
   INTEGER, PUBLIC, PARAMETER ::   jpr2di = 0   !: number of columns for extra outer halo 
   INTEGER, PUBLIC, PARAMETER ::   jpr2dj = 0   !: number of rows    for extra outer halo 

   ! halo with and starting/inding DO-loop indices
   INTEGER, PUBLIC ::   nn_hls   !: halo width (applies to both rows and columns)
   INTEGER, PUBLIC ::   Nis0, Nis1, Nis1nxt2, Nis2   !: start I-index (_0: without halo, _1 or _2: with 1 or 2 halos)
   INTEGER, PUBLIC ::   Nie0, Nie1, Nie1nxt2, Nie2   !: end   I-index (_0: without halo, _1 or _2: with 1 or 2 halos)
   INTEGER, PUBLIC ::   Njs0, Njs1, Njs1nxt2, Njs2   !: start J-index (_0: without halo, _1 or _2: with 1 or 2 halos)
   INTEGER, PUBLIC ::   Nje0, Nje1, Nje1nxt2, Nje2   !: end   J-index (_0: without halo, _1 or _2: with 1 or 2 halos)
   INTEGER, PUBLIC ::   Ni_0, Nj_0, Ni_1, Nj_1, Ni_2, Nj_2   !: domain size (_0: without halo, _1 or _2: with 1 or 2 halos)
   INTEGER, PUBLIC ::   Ni0glo, Nj0glo
   
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: par_oce.F90 10068 2018-08-28 14:09:04Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!======================================================================
END MODULE par_oce
