MODULE lbclnk
   !!======================================================================
   !!                       ***  MODULE  lbclnk  ***
   !! NEMO        : lateral boundary conditions
   !!=====================================================================
   !! History :  OPA  ! 1997-06  (G. Madec)  Original code
   !!   NEMO     1.0  ! 2002-09  (G. Madec)  F90: Free form and module
   !!            3.2  ! 2009-03  (R. Benshila)  External north fold treatment  
   !!            3.5  ! 2012     (S.Mocavero, I. Epicoco)  optimization of BDY comm. via lbc_bdy_lnk and lbc_obc_lnk
   !!            3.4  ! 2012-12  (R. Bourdalle-Badie, G. Reffray)  add a C1D case  
   !!            3.6  ! 2015-06  (O. Tintó and M. Castrillo)  add lbc_lnk_multi  
   !!            4.0  ! 2017-03  (G. Madec) automatique allocation of array size (use with any 3rd dim size)
   !!             -   ! 2017-04  (G. Madec) remove duplicated routines (lbc_lnk_2d_9, lbc_lnk_2d_multiple, lbc_lnk_3d_gather)
   !!             -   ! 2017-05  (G. Madec) create generic.h90 files to generate all lbc and north fold routines
   !!----------------------------------------------------------------------
   !!           define the generic interfaces of lib_mpp routines
   !!----------------------------------------------------------------------
   !!   lbc_lnk       : generic interface for mpp_lnk_3d and mpp_lnk_2d routines defined in lib_mpp
   !!   lbc_bdy_lnk   : generic interface for mpp_lnk_bdy_2d and mpp_lnk_bdy_3d routines defined in lib_mpp
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain
   USE lib_mpp        ! distributed memory computing library
   USE lbcnfd         ! north fold
   USE in_out_manager ! I/O manager

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_lnk
      MODULE PROCEDURE   mpp_lnk_2d_sp   , mpp_lnk_3d_sp   , mpp_lnk_4d_sp
      MODULE PROCEDURE   mpp_lnk_2d_dp   , mpp_lnk_3d_dp   , mpp_lnk_4d_dp
   END INTERFACE
   INTERFACE lbc_lnk_ptr
      MODULE PROCEDURE   mpp_lnk_2d_ptr_sp , mpp_lnk_3d_ptr_sp , mpp_lnk_4d_ptr_sp
      MODULE PROCEDURE   mpp_lnk_2d_ptr_dp , mpp_lnk_3d_ptr_dp , mpp_lnk_4d_ptr_dp
   END INTERFACE
   INTERFACE lbc_lnk_multi
      MODULE PROCEDURE   lbc_lnk_2d_multi_sp , lbc_lnk_3d_multi_sp, lbc_lnk_4d_multi_sp
      MODULE PROCEDURE   lbc_lnk_2d_multi_dp , lbc_lnk_3d_multi_dp, lbc_lnk_4d_multi_dp
   END INTERFACE
   !
   INTERFACE lbc_lnk_icb
      MODULE PROCEDURE mpp_lnk_2d_icb_dp, mpp_lnk_2d_icb_sp
   END INTERFACE

   INTERFACE mpp_nfd
      MODULE PROCEDURE   mpp_nfd_2d_sp    , mpp_nfd_3d_sp    , mpp_nfd_4d_sp
      MODULE PROCEDURE   mpp_nfd_2d_dp    , mpp_nfd_3d_dp    , mpp_nfd_4d_dp
      MODULE PROCEDURE   mpp_nfd_2d_ptr_sp, mpp_nfd_3d_ptr_sp, mpp_nfd_4d_ptr_sp
      MODULE PROCEDURE   mpp_nfd_2d_ptr_dp, mpp_nfd_3d_ptr_dp, mpp_nfd_4d_ptr_dp
      
   END INTERFACE

   PUBLIC   lbc_lnk       ! ocean/ice lateral boundary conditions
   PUBLIC   lbc_lnk_multi ! modified ocean/ice lateral boundary conditions
   PUBLIC   lbc_lnk_icb   ! iceberg lateral boundary conditions

#if   defined key_mpp_mpi
!$AGRIF_DO_NOT_TREAT
   INCLUDE 'mpif.h'
!$AGRIF_END_DO_NOT_TREAT
#endif

   INTEGER, PUBLIC, PARAMETER ::   jpfillnothing = 1
   INTEGER, PUBLIC, PARAMETER ::   jpfillcst     = 2
   INTEGER, PUBLIC, PARAMETER ::   jpfillcopy    = 3
   INTEGER, PUBLIC, PARAMETER ::   jpfillperio   = 4
   INTEGER, PUBLIC, PARAMETER ::   jpfillmpi     = 5

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: lbclnk.F90 13226 2020-07-02 14:24:31Z orioltp $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!                   ***   load_ptr_(2,3,4)d   ***
   !!
   !!   * Dummy Argument :
   !!       in    ==>   ptab       ! array to be loaded (2D, 3D or 4D)
   !!                   cd_nat     ! nature of pt2d array grid-points
   !!                   psgn       ! sign used across the north fold boundary
   !!       inout <=>   ptab_ptr   ! array of 2D, 3D or 4D pointers
   !!                   cdna_ptr   ! nature of ptab array grid-points
   !!                   psgn_ptr   ! sign used across the north fold boundary
   !!                   kfld       ! number of elements that has been attributed
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!                  ***   lbc_lnk_(2,3,4)d_multi   ***
   !!                     ***   load_ptr_(2,3,4)d   ***
   !!
   !!   * Argument : dummy argument use in lbc_lnk_multi_... routines
   !!
   !!----------------------------------------------------------------------

   !!
   !!   ----   SINGLE PRECISION VERSIONS
   !!
#  define SINGLE_PRECISION
#  define DIM_2d
#     define ROUTINE_LOAD           load_ptr_2d_sp
#     define ROUTINE_MULTI          lbc_lnk_2d_multi_sp
#     include "lbc_lnk_multi_generic.h90"
#     undef ROUTINE_MULTI
#     undef ROUTINE_LOAD
#  undef DIM_2d

#  define DIM_3d
#     define ROUTINE_LOAD           load_ptr_3d_sp
#     define ROUTINE_MULTI          lbc_lnk_3d_multi_sp
#     include "lbc_lnk_multi_generic.h90"
#     undef ROUTINE_MULTI
#     undef ROUTINE_LOAD
#  undef DIM_3d

#  define DIM_4d
#     define ROUTINE_LOAD           load_ptr_4d_sp
#     define ROUTINE_MULTI          lbc_lnk_4d_multi_sp
#     include "lbc_lnk_multi_generic.h90"
#     undef ROUTINE_MULTI
#     undef ROUTINE_LOAD
#  undef DIM_4d
#  undef SINGLE_PRECISION
   !!
   !!   ----   DOUBLE PRECISION VERSIONS
   !!

#  define DIM_2d
#     define ROUTINE_LOAD           load_ptr_2d_dp
#     define ROUTINE_MULTI          lbc_lnk_2d_multi_dp
#     include "lbc_lnk_multi_generic.h90"
#     undef ROUTINE_MULTI
#     undef ROUTINE_LOAD
#  undef DIM_2d

#  define DIM_3d
#     define ROUTINE_LOAD           load_ptr_3d_dp
#     define ROUTINE_MULTI          lbc_lnk_3d_multi_dp
#     include "lbc_lnk_multi_generic.h90"
#     undef ROUTINE_MULTI
#     undef ROUTINE_LOAD
#  undef DIM_3d

#  define DIM_4d
#     define ROUTINE_LOAD           load_ptr_4d_dp
#     define ROUTINE_MULTI          lbc_lnk_4d_multi_dp
#     include "lbc_lnk_multi_generic.h90"
#     undef ROUTINE_MULTI
#     undef ROUTINE_LOAD
#  undef DIM_4d

   !!----------------------------------------------------------------------
   !!                   ***  routine mpp_lnk_(2,3,4)d  ***
   !!
   !!   * Argument : dummy argument use in mpp_lnk_... routines
   !!                ptab      :   array or pointer of arrays on which the boundary condition is applied
   !!                cd_nat    :   nature of array grid-points
   !!                psgn      :   sign used across the north fold boundary
   !!                kfld      :   optional, number of pt3d arrays
   !!                kfillmode :   optional, method to be use to fill the halos (see jpfill* variables)
   !!                pfillval  :   optional, background value (used with jpfillcopy)
   !!----------------------------------------------------------------------
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
   !!
   !!   ----   SINGLE PRECISION VERSIONS
   !!
# define SINGLE_PRECISION
#  define DIM_2d
#     define ROUTINE_LNK           mpp_lnk_2d_sp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     define MULTI
#     define ROUTINE_LNK           mpp_lnk_2d_ptr_sp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     undef MULTI
#  undef DIM_2d
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
#  define DIM_3d
#     define ROUTINE_LNK           mpp_lnk_3d_sp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     define MULTI
#     define ROUTINE_LNK           mpp_lnk_3d_ptr_sp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     undef MULTI
#  undef DIM_3d
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
#  define DIM_4d
#     define ROUTINE_LNK           mpp_lnk_4d_sp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     define MULTI
#     define ROUTINE_LNK           mpp_lnk_4d_ptr_sp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     undef MULTI
#  undef DIM_4d
# undef SINGLE_PRECISION

   !!
   !!   ----   DOUBLE PRECISION VERSIONS
   !!
#  define DIM_2d
#     define ROUTINE_LNK           mpp_lnk_2d_dp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     define MULTI
#     define ROUTINE_LNK           mpp_lnk_2d_ptr_dp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     undef MULTI
#  undef DIM_2d
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
#  define DIM_3d
#     define ROUTINE_LNK           mpp_lnk_3d_dp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     define MULTI
#     define ROUTINE_LNK           mpp_lnk_3d_ptr_dp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     undef MULTI
#  undef DIM_3d
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
#  define DIM_4d
#     define ROUTINE_LNK           mpp_lnk_4d_dp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     define MULTI
#     define ROUTINE_LNK           mpp_lnk_4d_ptr_dp
#     include "mpp_lnk_generic.h90"
#     undef ROUTINE_LNK
#     undef MULTI
#  undef DIM_4d


   !!----------------------------------------------------------------------
   !!                   ***  routine mpp_nfd_(2,3,4)d  ***
   !!
   !!   * Argument : dummy argument use in mpp_nfd_... routines
   !!                ptab      :   array or pointer of arrays on which the boundary condition is applied
   !!                cd_nat    :   nature of array grid-points
   !!                psgn      :   sign used across the north fold boundary
   !!                kfld      :   optional, number of pt3d arrays
   !!                kfillmode :   optional, method to be use to fill the halos (see jpfill* variables)
   !!                pfillval  :   optional, background value (used with jpfillcopy)
   !!----------------------------------------------------------------------
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
   !!
   !!   ----   SINGLE PRECISION VERSIONS
   !!
#  define SINGLE_PRECISION
#  define DIM_2d
#     define ROUTINE_NFD           mpp_nfd_2d_sp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           mpp_nfd_2d_ptr_sp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_2d
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
#  define DIM_3d
#     define ROUTINE_NFD           mpp_nfd_3d_sp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           mpp_nfd_3d_ptr_sp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_3d
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
#  define DIM_4d
#     define ROUTINE_NFD           mpp_nfd_4d_sp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           mpp_nfd_4d_ptr_sp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_4d
#  undef SINGLE_PRECISION

   !!
   !!   ----   DOUBLE PRECISION VERSIONS
   !!
#  define DIM_2d
#     define ROUTINE_NFD           mpp_nfd_2d_dp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           mpp_nfd_2d_ptr_dp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_2d
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
#  define DIM_3d
#     define ROUTINE_NFD           mpp_nfd_3d_dp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           mpp_nfd_3d_ptr_dp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_3d
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
#  define DIM_4d
#     define ROUTINE_NFD           mpp_nfd_4d_dp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           mpp_nfd_4d_ptr_dp
#     include "mpp_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_4d

   !!======================================================================


   !!======================================================================
     !!---------------------------------------------------------------------
      !!                   ***  routine mpp_lbc_north_icb  ***
      !!
      !! ** Purpose :   Ensure proper north fold horizontal bondary condition
      !!              in mpp configuration in case of jpn1 > 1 and for 2d
      !!              array with outer extra halo
      !!
      !! ** Method  :   North fold condition and mpp with more than one proc
      !!              in i-direction require a specific treatment. We gather
      !!              the 4+kextj northern lines of the global domain on 1
      !!              processor and apply lbc north-fold on this sub array.
      !!              Then we scatter the north fold array back to the processors.
      !!              This routine accounts for an extra halo with icebergs
      !!              and assumes ghost rows and columns have been suppressed.
      !!
      !!----------------------------------------------------------------------
#     define SINGLE_PRECISION
#     define ROUTINE_LNK           mpp_lbc_north_icb_sp
#     include "mpp_lbc_north_icb_generic.h90"
#     undef ROUTINE_LNK
#     undef SINGLE_PRECISION
#     define ROUTINE_LNK           mpp_lbc_north_icb_dp
#     include "mpp_lbc_north_icb_generic.h90"
#     undef ROUTINE_LNK
 

      !!----------------------------------------------------------------------
      !!                  ***  routine mpp_lnk_2d_icb  ***
      !!
      !! ** Purpose :   Message passing management for 2d array (with extra halo for icebergs)
      !!                This routine receives a (1-kexti:jpi+kexti,1-kexti:jpj+kextj)
      !!                array (usually (0:jpi+1, 0:jpj+1)) from lbc_lnk_icb calls.
      !!
      !! ** Method  :   Use mppsend and mpprecv function for passing mask
      !!      between processors following neighboring subdomains.
      !!            domain parameters
      !!                    jpi    : first dimension of the local subdomain
      !!                    jpj    : second dimension of the local subdomain
      !!                    kexti  : number of columns for extra outer halo
      !!                    kextj  : number of rows for extra outer halo
      !!                    nbondi : mark for "east-west local boundary"
      !!                    nbondj : mark for "north-south local boundary"
      !!                    noea   : number for local neighboring processors
      !!                    nowe   : number for local neighboring processors
      !!                    noso   : number for local neighboring processors
      !!                    nono   : number for local neighboring processors
      !!----------------------------------------------------------------------

#     define SINGLE_PRECISION
#     define ROUTINE_LNK           mpp_lnk_2d_icb_sp
#     include "mpp_lnk_icb_generic.h90"
#     undef ROUTINE_LNK
#     undef SINGLE_PRECISION
#     define ROUTINE_LNK           mpp_lnk_2d_icb_dp
#     include "mpp_lnk_icb_generic.h90"
#     undef ROUTINE_LNK
  
END MODULE lbclnk

