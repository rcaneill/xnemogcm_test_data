MODULE agrif_domzgr

   USE agrif_profiles
   USE dom_oce

   IMPLICIT NONE
   PRIVATE
  
   PUBLIC :: agrif_create_bathy_meter
 

CONTAINS

#if defined key_agrif

   SUBROUTINE agrif_create_bathy_meter

      CALL Agrif_Init_variable(bathy_id, procname = init_bathy)

   END SUBROUTINE agrif_create_bathy_meter

   SUBROUTINE init_bathy( ptab, i1, i2, j1, j2, before, nb,ndir)
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      !
      !!----------------------------------------------------------------------  
      INTEGER :: ji,jj

      IF( before) THEN
         ptab(i1:i2,j1:j2) = bathy(i1:i2,j1:j2)
         DO jj=j1,j2
            DO ji=i1,i2
               ptab(ji,jj) = SUM( e3t_0(ji,jj, 1:mbkt(ji,jj) ) ) * ssmask(ji,jj)
            END DO
         END DO
      ELSE
         bathy(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_bathy

#else
   SUBROUTINE agrif_create_bathy_meter
   END SUBROUTINE agrif_create_bathy_meter
#endif
END MODULE agrif_domzgr
