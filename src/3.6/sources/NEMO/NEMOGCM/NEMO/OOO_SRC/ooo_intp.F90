MODULE ooo_intp
   !!======================================================================
   !!                    ***  MODULE ooo_intp ***
   !! ** Purpose : Run NEMO observation operator in offline mode
   !!======================================================================
   !! NEMO modules
   USE in_out_manager
   USE diaobs
   !! Offline obs_oper modules
   USE ooo_read
   USE ooo_data

   IMPLICIT NONE
   PRIVATE

   PUBLIC ooo_interp

   !! $Id: ooo_intp.F90 5215 2015-04-15 16:11:56Z nicolasmartin $
   CONTAINS

      SUBROUTINE ooo_interp
         !!----------------------------------------------------------------------
         !!                    ***  SUBROUTINE ooo_interp ***
         !!
         !! ** Purpose : To interpolate the model as if it were running online.
         !!
         !! ** Method : 1. Populate model counterparts
         !!             2. Call dia_obs at appropriate time steps
         !!----------------------------------------------------------------------
         INTEGER :: istp ! time step index
         !! Loop over entire run
         istp = nit000
         nstop = 0
         DO WHILE ( istp <= nitend .AND. nstop == 0 )
            IF (jifile <= n_files + 1) THEN
               IF ( MOD(istp, nn_ooo_freq) == nit000 ) THEN
                  !! Read next model counterpart
                  CALL ooo_rea_dri(jifile)
                  jifile = jifile + 1
               ENDIF
               !! Interpolate single time step
               CALL dia_obs(istp)
            ENDIF
            !! Increment model step
            istp = istp + 1
         END DO
      END SUBROUTINE ooo_interp

END MODULE ooo_intp
