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
#if ! defined key_mpi_off
   USE MPI
#endif

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_lnk
      MODULE PROCEDURE   lbc_lnk_call_2d_sp, lbc_lnk_call_3d_sp, lbc_lnk_call_4d_sp
      MODULE PROCEDURE   lbc_lnk_call_2d_dp, lbc_lnk_call_3d_dp, lbc_lnk_call_4d_dp
   END INTERFACE

   INTERFACE lbc_lnk_pt2pt
      MODULE PROCEDURE   lbc_lnk_pt2pt_sp, lbc_lnk_pt2pt_dp
   END INTERFACE

   INTERFACE lbc_lnk_neicoll
      MODULE PROCEDURE   lbc_lnk_neicoll_sp ,lbc_lnk_neicoll_dp
   END INTERFACE
   !
   INTERFACE lbc_lnk_icb
      MODULE PROCEDURE mpp_lnk_2d_icb_dp, mpp_lnk_2d_icb_sp
   END INTERFACE

   PUBLIC   lbc_lnk            ! ocean/ice lateral boundary conditions
   PUBLIC   lbc_lnk_icb        ! iceberg lateral boundary conditions

   REAL(dp), DIMENSION(:), ALLOCATABLE ::   buffsnd_dp, buffrcv_dp   ! MPI send/recv buffers
   REAL(sp), DIMENSION(:), ALLOCATABLE ::   buffsnd_sp, buffrcv_sp   ! 
   INTEGER,  DIMENSION(8)              ::   nreq_p2p                 ! request id for MPI_Isend in point-2-point communication
   
   !! * Substitutions
   !!#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: lbclnk.F90 14433 2021-02-11 08:06:49Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!                   ***   lbc_lnk_call_[234]d_[sd]p   ***
   !!
   !!   * Dummy Argument :
   !!       in    ==>   cdname     ! name of the calling subroutine (for monitoring)
   !!                   ptab       ! array to be loaded (2D, 3D or 4D)
   !!                   cd_nat     ! nature of pt2d array grid-points
   !!                   psgn       ! sign used across the north fold boundary
   !!       inout <=>   ptab_ptr   ! array of 2D, 3D or 4D pointers
   !!                   cdna_ptr   ! nature of ptab array grid-points
   !!                   psgn_ptr   ! sign used across the north fold boundary
   !!                   kfld       ! number of elements that has been attributed
   !!----------------------------------------------------------------------
   !
   !!----------------------------------------------------------------------
   !!
   !!                  ***   lbc_lnk_call_[234]d_[sd]p   ***
   !!                  ***     load_ptr_[234]d_[sd]p     ***
   !!
   !!----------------------------------------------------------------------
   !!
   !!   ----   SINGLE PRECISION VERSIONS
   !!
#define PRECISION sp
# define DIM_2d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_2d
# define DIM_3d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_3d
# define DIM_4d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_4d
#undef PRECISION
   !!
   !!   ----   DOUBLE PRECISION VERSIONS
   !!
#define PRECISION dp
# define DIM_2d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_2d
# define DIM_3d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_3d
# define DIM_4d
#    include "lbc_lnk_call_generic.h90"
# undef  DIM_4d
#undef PRECISION
   !
   !!----------------------------------------------------------------------
   !!                   ***  lbc_lnk_pt2pt_[sd]p  ***
   !!                  ***  lbc_lnk_neicoll_[sd]p  ***
   !!
   !!   * Argument : dummy argument use in lbc_lnk_... routines
   !!                cdname    :   name of the calling subroutine (for monitoring)
   !!                ptab      :   pointer of arrays on which the boundary condition is applied
   !!                cd_nat    :   nature of array grid-points
   !!                psgn      :   sign used across the north fold boundary
   !!                kfld      :   number of pt3d arrays
   !!                kfillmode :   optional, method to be use to fill the halos (see jpfill* variables)
   !!                pfillval  :   optional, background value (used with jpfillcopy)
   !!----------------------------------------------------------------------
   !!
   !!   ----   SINGLE PRECISION VERSIONS
   !!
#define PRECISION sp
#  define MPI_TYPE MPI_REAL
#  define BUFFSND buffsnd_sp
#  define BUFFRCV buffrcv_sp
#  include "lbc_lnk_pt2pt_generic.h90"
#  include "lbc_lnk_neicoll_generic.h90"
#  undef MPI_TYPE
#  undef BUFFSND
#  undef BUFFRCV
#undef PRECISION
   !!
   !!   ----   DOUBLE PRECISION VERSIONS
   !!
#define PRECISION dp
#  define MPI_TYPE MPI_DOUBLE_PRECISION
#  define BUFFSND buffsnd_dp
#  define BUFFRCV buffrcv_dp
#  include "lbc_lnk_pt2pt_generic.h90"
#  include "lbc_lnk_neicoll_generic.h90"
#  undef MPI_TYPE
#  undef BUFFSND
#  undef BUFFRCV
#undef PRECISION

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
      !!                    mpinei : number of neighboring domains (starting at 0, -1 if no neighbourg)
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
