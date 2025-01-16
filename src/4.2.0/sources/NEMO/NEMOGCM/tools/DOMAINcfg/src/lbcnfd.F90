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
   !!   lbc_nfd       : generic interface for lbc_nfd_3d and lbc_nfd_2d routines
   !!   lbc_nfd_3d    : lateral boundary condition: North fold treatment for a 3D arrays   (lbc_nfd)
   !!   lbc_nfd_2d    : lateral boundary condition: North fold treatment for a 2D arrays   (lbc_nfd)
   !!   lbc_nfd_nogather       : generic interface for lbc_nfd_nogather_3d and 
   !!                            lbc_nfd_nogather_2d routines (designed for use
   !!                            with ln_nnogather to avoid global width arrays
   !!                            mpi all gather operations)
   !!----------------------------------------------------------------------
   USE dom_oce        ! ocean space and time domain 
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   INTERFACE lbc_nfd
      MODULE PROCEDURE   lbc_nfd_2d_sp    , lbc_nfd_3d_sp    , lbc_nfd_4d_sp
      MODULE PROCEDURE   lbc_nfd_2d_ptr_sp, lbc_nfd_3d_ptr_sp, lbc_nfd_4d_ptr_sp
      MODULE PROCEDURE   lbc_nfd_2d_ext_sp
      MODULE PROCEDURE   lbc_nfd_2d_dp    , lbc_nfd_3d_dp    , lbc_nfd_4d_dp
      MODULE PROCEDURE   lbc_nfd_2d_ptr_dp, lbc_nfd_3d_ptr_dp, lbc_nfd_4d_ptr_dp
      MODULE PROCEDURE   lbc_nfd_2d_ext_dp
   END INTERFACE
   !
   INTERFACE lbc_nfd_nogather
!                        ! Currently only 4d array version is needed
     MODULE PROCEDURE   lbc_nfd_nogather_2d_sp    , lbc_nfd_nogather_3d_sp
     MODULE PROCEDURE   lbc_nfd_nogather_4d_sp
     MODULE PROCEDURE   lbc_nfd_nogather_2d_ptr_sp, lbc_nfd_nogather_3d_ptr_sp
     MODULE PROCEDURE   lbc_nfd_nogather_2d_dp    , lbc_nfd_nogather_3d_dp
     MODULE PROCEDURE   lbc_nfd_nogather_4d_dp
     MODULE PROCEDURE   lbc_nfd_nogather_2d_ptr_dp, lbc_nfd_nogather_3d_ptr_dp
!     MODULE PROCEDURE   lbc_nfd_nogather_4d_ptr
   END INTERFACE

   TYPE, PUBLIC ::   PTR_2D_dp   !: array of 2D pointers (also used in lib_mpp)
      REAL(dp), DIMENSION (:,:)    , POINTER ::   pt2d
   END TYPE PTR_2D_dp
   TYPE, PUBLIC ::   PTR_3D_dp   !: array of 3D pointers (also used in lib_mpp)
      REAL(dp), DIMENSION (:,:,:)  , POINTER ::   pt3d
   END TYPE PTR_3D_dp
   TYPE, PUBLIC ::   PTR_4D_dp   !: array of 4D pointers (also used in lib_mpp)
      REAL(dp), DIMENSION (:,:,:,:), POINTER ::   pt4d
   END TYPE PTR_4D_dp

   TYPE, PUBLIC ::   PTR_2D_sp   !: array of 2D pointers (also used in lib_mpp)
      REAL(sp), DIMENSION (:,:)    , POINTER ::   pt2d
   END TYPE PTR_2D_sp
   TYPE, PUBLIC ::   PTR_3D_sp   !: array of 3D pointers (also used in lib_mpp)
      REAL(sp), DIMENSION (:,:,:)  , POINTER ::   pt3d
   END TYPE PTR_3D_sp
   TYPE, PUBLIC ::   PTR_4D_sp   !: array of 4D pointers (also used in lib_mpp)
      REAL(sp), DIMENSION (:,:,:,:), POINTER ::   pt4d
   END TYPE PTR_4D_sp


   PUBLIC   lbc_nfd            ! north fold conditions
   PUBLIC   lbc_nfd_nogather   ! north fold conditions (no allgather case)

   INTEGER, PUBLIC, PARAMETER            ::   jpmaxngh = 3               !:
   INTEGER, PUBLIC                       ::   nsndto                     !:
   INTEGER, PUBLIC, DIMENSION (jpmaxngh) ::   isendto                    !: processes to which communicate
   INTEGER, PUBLIC                       ::   ijpj

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: lbcnfd.F90 13286 2020-07-09 15:48:29Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   !!----------------------------------------------------------------------
   !!                   ***  routine lbc_nfd_(2,3,4)d  ***
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
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
#  define SINGLE_PRECISION
#  define DIM_2d
#     define ROUTINE_NFD           lbc_nfd_2d_sp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_2d_ptr_sp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_2d
   !
   !                       !==  2D array with extra haloes  ==!
   !
#  define DIM_2d
#     define ROUTINE_NFD           lbc_nfd_2d_ext_sp
#     include "lbc_nfd_ext_generic.h90"
#     undef ROUTINE_NFD
#  undef DIM_2d
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
#  define DIM_3d
#     define ROUTINE_NFD           lbc_nfd_3d_sp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_3d_ptr_sp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_3d
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
#  define DIM_4d
#     define ROUTINE_NFD           lbc_nfd_4d_sp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_4d_ptr_sp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_4d
   !
   !  lbc_nfd_nogather routines
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
#  define DIM_2d
#     define ROUTINE_NFD           lbc_nfd_nogather_2d_sp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_nogather_2d_ptr_sp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_2d
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
#  define DIM_3d
#     define ROUTINE_NFD           lbc_nfd_nogather_3d_sp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_nogather_3d_ptr_sp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_3d
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
#  define DIM_4d
#     define ROUTINE_NFD           lbc_nfd_nogather_4d_sp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
!#     define MULTI
!#     define ROUTINE_NFD           lbc_nfd_nogather_4d_ptr
!#     include "lbc_nfd_nogather_generic.h90"
!#     undef ROUTINE_NFD
!#     undef MULTI
#  undef DIM_4d
#  undef SINGLE_PRECISION

   !!----------------------------------------------------------------------
   !
   !                       !==  DOUBLE PRECISION VERSIONS
   !
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
#  define DIM_2d
#     define ROUTINE_NFD           lbc_nfd_2d_dp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_2d_ptr_dp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_2d
   !
   !                       !==  2D array with extra haloes  ==!
   !
#  define DIM_2d
#     define ROUTINE_NFD           lbc_nfd_2d_ext_dp
#     include "lbc_nfd_ext_generic.h90"
#     undef ROUTINE_NFD
#  undef DIM_2d
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
#  define DIM_3d
#     define ROUTINE_NFD           lbc_nfd_3d_dp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_3d_ptr_dp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_3d
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
#  define DIM_4d
#     define ROUTINE_NFD           lbc_nfd_4d_dp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_4d_ptr_dp
#     include "lbc_nfd_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_4d
   !
   !  lbc_nfd_nogather routines
   !
   !                       !==  2D array and array of 2D pointer  ==!
   !
#  define DIM_2d
#     define ROUTINE_NFD           lbc_nfd_nogather_2d_dp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_nogather_2d_ptr_dp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_2d
   !
   !                       !==  3D array and array of 3D pointer  ==!
   !
#  define DIM_3d
#     define ROUTINE_NFD           lbc_nfd_nogather_3d_dp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
#     define MULTI
#     define ROUTINE_NFD           lbc_nfd_nogather_3d_ptr_dp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
#     undef MULTI
#  undef DIM_3d
   !
   !                       !==  4D array and array of 4D pointer  ==!
   !
#  define DIM_4d
#     define ROUTINE_NFD           lbc_nfd_nogather_4d_dp
#     include "lbc_nfd_nogather_generic.h90"
#     undef ROUTINE_NFD
!#     define MULTI
!#     define ROUTINE_NFD           lbc_nfd_nogather_4d_ptr
!#     include "lbc_nfd_nogather_generic.h90"
!#     undef ROUTINE_NFD
!#     undef MULTI
#  undef DIM_4d

   !!----------------------------------------------------------------------



   !!======================================================================
END MODULE lbcnfd
