MODULE halo_mng
   !!======================================================================
   !!                       ***  MODULE  halo_mng  ***
   !! Ocean numerics:  massively parallel processing library
   !!=====================================================================
   !! History :  OPA  !  1994  (M. Guyon, J. Escobar, M. Imbard)
   !Original code
   !!            4.0  !  2019  (CMCC - ASC)  initial version of halo management module
   !in_out_manager
   !!----------------------------------------------------------------------

   USE dom_oce       ! ocean space and time domain
   USE lbclnk        ! ocean lateral boundary condition (or mpp link) 

   IMPLICIT NONE
   PRIVATE

   INTERFACE halo_mng_resize
      MODULE PROCEDURE halo_mng_resize_2D, halo_mng_resize_3D, halo_mng_resize_4D, halo_mng_resize_5D
   END INTERFACE

   PUBLIC halo_mng_resize
   PUBLIC halo_mng_init
   PUBLIC halo_mng_set

   INTEGER :: jpi_1, jpj_1
   INTEGER :: jpimax_1, jpjmax_1
   INTEGER :: Nis0_1, Njs0_1
   INTEGER :: Nie0_1, Nje0_1
CONTAINS

   SUBROUTINE halo_mng_init( )

        jpi_1 = jpi
        jpj_1 = jpj

        Nis0_1 = Nis0
        Njs0_1 = Njs0

        Nie0_1 = Nie0
        Nje0_1 = Nje0

   	jpimax_1 = jpimax
   	jpjmax_1 = jpjmax

   END SUBROUTINE halo_mng_init

   SUBROUTINE halo_mng_set( khls )
   
        INTEGER, INTENT(in   )    ::   khls

        nn_hls = khls

        jpi = jpi_1 + 2*khls -2
        jpj = jpj_1 + 2*khls -2

        jpi = jpi_1 + 2*khls -2
        jpj = jpj_1 + 2*khls -2
        
        jpimax = jpimax_1 + 2*khls -2
        jpjmax = jpjmax_1 + 2*khls -2

        Nis0 = Nis0_1 + khls - 1
        Njs0 = Njs0_1 + khls - 1

        Nie0 = Nie0_1 + khls - 1
        Nje0 = Nje0_1 + khls - 1

   END SUBROUTINE halo_mng_set
   
	SUBROUTINE halo_mng_resize_2D(pta, cdna, psgn, fillval)
	
		REAL(wp), POINTER, DIMENSION(:,:) :: pta
		CHARACTER(len=1), INTENT(in)  :: cdna
		REAL(wp), INTENT(in)  :: psgn
		REAL(wp), OPTIONAL, INTENT(in ) :: fillval
		REAL(wp), POINTER, DIMENSION(:,:) :: zpta
		INTEGER :: offset
		INTEGER :: pta_size_i, pta_size_j

		pta_size_i = SIZE(pta,1)
		pta_size_j = SIZE(pta,2)
		
		! check if the current size of pta is equal to the current expected dimension
		IF (pta_size_i .ne. jpi) THEN
			ALLOCATE (zpta(jpi, jpj))
			offset = abs((jpi - pta_size_i) / 2) 

			IF (pta_size_i .lt. jpi) THEN
				zpta (offset+1 : offset+pta_size_i, offset+1 : offset+pta_size_j) = pta
			ELSE
				zpta = pta(offset+1 : offset+jpi, offset+1 : offset+jpj)
			END IF
			CALL lbc_lnk( 'halo_mng_resize_2D', zpta, cdna, psgn, pfillval=fillval)
			DEALLOCATE(pta)
			pta => zpta
		END IF
		
	END SUBROUTINE halo_mng_resize_2D

	SUBROUTINE halo_mng_resize_3D(pta, cdna, psgn, fillval)
	
		REAL(wp), POINTER, DIMENSION(:,:,:) :: pta
		CHARACTER(len=1), INTENT(in)  :: cdna
		REAL(wp), INTENT(in)  :: psgn
		REAL(wp), OPTIONAL, INTENT(in ) :: fillval
		REAL(wp), POINTER, DIMENSION(:,:,:) :: zpta
		INTEGER :: offset
		INTEGER :: pta_size_i, pta_size_j

		pta_size_i = SIZE(pta,1)
		pta_size_j = SIZE(pta,2)
		
		! check if the current size of pta is equal to the current expected dimension
		IF (pta_size_i .ne. jpi) THEN
			ALLOCATE (zpta(jpi, jpj, jpk))
			offset = abs((jpi - pta_size_i) / 2) 

			IF (pta_size_i .lt. jpi) THEN
				zpta (offset+1 : offset+pta_size_i, offset+1 : offset+pta_size_j, :) = pta
			ELSE
				zpta = pta(offset+1 : offset+jpi, offset+1 : offset+jpj, :)
			END IF
			CALL lbc_lnk( 'halo_mng_resize_3D', zpta, cdna, psgn, pfillval=fillval)
			DEALLOCATE(pta)
			pta => zpta
		END IF
		
	END SUBROUTINE halo_mng_resize_3D

	SUBROUTINE halo_mng_resize_4D(pta, cdna, psgn, fillval, fjpt)
	
		REAL(wp), POINTER, DIMENSION(:,:,:,:) :: pta
		CHARACTER(len=1), INTENT(in)  :: cdna
		REAL(wp), INTENT(in)  :: psgn
		REAL(wp), OPTIONAL, INTENT(in) :: fillval
		INTEGER , INTENT(in) ::   fjpt 
		REAL(wp), POINTER, DIMENSION(:,:,:,:) :: zpta
		INTEGER :: offset
		INTEGER :: pta_size_i, pta_size_j

		pta_size_i = SIZE(pta,1)
		pta_size_j = SIZE(pta,2)
		
		! check if the current size of pta is equal to the current expected dimension
		IF (pta_size_i .ne. jpi) THEN
			ALLOCATE (zpta(jpi, jpj, jpk, jpt))
			offset = abs((jpi - pta_size_i) / 2) 

			IF (pta_size_i .lt. jpi) THEN
				zpta (offset+1 : offset+pta_size_i, offset+1 : offset+pta_size_j, :, :) = pta
			ELSE
				zpta = pta(offset+1 : offset+jpi, offset+1 : offset+jpj, :, :)
			END IF
			CALL lbc_lnk( 'halo_mng_resize_4D', zpta(:,:,:,fjpt), cdna, psgn, pfillval=fillval)
			DEALLOCATE(pta)
			pta => zpta
		END IF
		
	END SUBROUTINE halo_mng_resize_4D
	
	SUBROUTINE halo_mng_resize_5D(pta, cdna, psgn, fillval, kjpt, fjpt)
	
		REAL(wp), POINTER, DIMENSION(:,:,:,:,:) :: pta
		CHARACTER(len=1), INTENT(in)  :: cdna
		REAL(wp), INTENT(in)  :: psgn
		REAL(wp), OPTIONAL, INTENT(in) :: fillval
		INTEGER , OPTIONAL, INTENT(in) :: kjpt            ! number of tracers
		INTEGER , INTENT(in) :: fjpt            
		REAL(wp), POINTER, DIMENSION(:,:,:,:,:) :: zpta
		INTEGER :: offset
		INTEGER :: pta_size_i, pta_size_j

		pta_size_i = SIZE(pta,1)
		pta_size_j = SIZE(pta,2)
		
		! check if the current size of pta is equal to the current expected dimension
		IF (pta_size_i .ne. jpi) THEN
			ALLOCATE (zpta(jpi, jpj, jpk, kjpt, jpt))
			offset = abs((jpi - pta_size_i) / 2) 

			IF (pta_size_i .lt. jpi) THEN
				zpta (offset+1 : offset+pta_size_i, offset+1 : offset+pta_size_j, :, :, :) = pta
			ELSE
				zpta = pta(offset+1 : offset+jpi, offset+1 : offset+jpj, :, :, :)
			END IF
			CALL lbc_lnk( 'halo_mng_resize_5D', zpta(:,:,:,:,fjpt), cdna, psgn, pfillval=fillval)
			DEALLOCATE(pta)
			pta => zpta
		END IF
		
	END SUBROUTINE halo_mng_resize_5D
	
END MODULE
