MODULE lbcnfd
   !!======================================================================
   !!                       ***  MODULE  lbcnfd  ***
   !! Ocean        : north fold  boundary conditions
   !!======================================================================
   !! History :  3.2  ! 2009-03  (R. Benshila)  Original code 
   !!            3.5  ! 2013-07  (I. Epicoco, S. Mocavero - CMCC) MPP optimization
   !!            4.0  ! 2017-04  (G. Madec) automatique allocation of array argument (use any 3rd dimension)
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   lbc_nfd       : generic interface for lbc_nfd_sp and lbc_nfd_dp routines that is doing the north fold in a non-mpi case 
   !!   mpp_nfd       : generic interface for mpp_nfd_sp and mpp_nfd_dp routines that will use lbc_nfd directly or indirectly
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain 
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
#if ! defined key_mpi_off
   USE MPI
#endif

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_nfd            ! called by mpp_nfd, lbc_lnk_pt2pt or lbc_lnk_neicoll
      MODULE PROCEDURE   lbc_nfd_sp, lbc_nfd_ext_sp
      MODULE PROCEDURE   lbc_nfd_dp, lbc_nfd_ext_dp
   END INTERFACE

   INTERFACE mpp_nfd            ! called by lbc_lnk_pt2pt or lbc_lnk_neicoll
      MODULE PROCEDURE   mpp_nfd_sp, mpp_nfd_dp
   END INTERFACE
   
   PUBLIC   mpp_nfd            ! mpi north fold conditions
   PUBLIC   lbc_nfd            ! north fold conditions

   INTEGER, PUBLIC                               :: nfd_nbnei
   INTEGER, PUBLIC, ALLOCATABLE, DIMENSION (:  ) :: nfd_rknei
   INTEGER, PUBLIC, ALLOCATABLE, DIMENSION (:,:) :: nfd_rksnd
   INTEGER, PUBLIC, ALLOCATABLE, DIMENSION (:,:) :: nfd_jisnd
   
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: lbcnfd.F90 15267 2021-09-17 09:04:34Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!                   ***  routine lbc_nfd_[sd]p  ***
   !!                   ***  routine lbc_nfd_ext_[sd]p  ***
   !!----------------------------------------------------------------------
   !!
   !! ** Purpose :   lateral boundary condition 
   !!                North fold treatment without processor exchanges. 
   !!
   !! ** Method  :   
   !!
   !! ** Action  :   ptab with updated values along the north fold
   !!----------------------------------------------------------------------
   !
   !                       !==  SINGLE PRECISION VERSIONS
   !
#define PRECISION sp
#  include "lbc_nfd_generic.h90"
#  include "lbc_nfd_ext_generic.h90"
#undef PRECISION
   !
   !                       !==  DOUBLE PRECISION VERSIONS
   !
#define PRECISION dp
#  include "lbc_nfd_generic.h90"
#  include "lbc_nfd_ext_generic.h90"
#undef PRECISION

   !!======================================================================
   !
   !!----------------------------------------------------------------------
   !!                   ***  routine mpp_nfd_[sd]p  ***
   !!
   !!   * Argument : dummy argument use in mpp_nfd_... routines
   !!                ptab      :   pointer of arrays on which the boundary condition is applied
   !!                cd_nat    :   nature of array grid-points
   !!                psgn      :   sign used across the north fold boundary
   !!                kfld      :   optional, number of pt3d arrays
   !!                kfillmode :   optional, method to be use to fill the halos (see jpfill* variables)
   !!                pfillval  :   optional, background value (used with jpfillcopy)
   !!----------------------------------------------------------------------
   !!
   !!   ----   SINGLE PRECISION VERSIONS
   !!
#define PRECISION sp
#  define MPI_TYPE MPI_REAL
#  include "mpp_nfd_generic.h90"
#  undef MPI_TYPE
#undef PRECISION
   !!
   !!   ----   DOUBLE PRECISION VERSIONS
   !!
#define PRECISION dp
#  define MPI_TYPE MPI_DOUBLE_PRECISION
#  include "mpp_nfd_generic.h90"
#  undef MPI_TYPE
#undef PRECISION

   !!======================================================================
END MODULE lbcnfd
