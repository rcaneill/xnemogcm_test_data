#define PPR_LIB      /* USE PPR library */
MODULE vremap
!$AGRIF_DO_NOT_TREAT
   !!======================================================================
   !!                       ***  MODULE  vremap  ***
   !! Ocean physics:  Vertical remapping routines
   !!
   !!======================================================================
   !! History : 4.0  !  2019-09  (Jérôme Chanut)  Original code
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!
   !!----------------------------------------------------------------------
   USE par_oce
#if defined PPR_LIB
   USE ppr_1d   ! D. Engwirda piecewise polynomial reconstruction library
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   reconstructandremap, remap_linear

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: vremap 11573 2019-09-19 09:18:03Z jchanut $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if ! defined PPR_LIB
   SUBROUTINE reconstructandremap(ptin, phin, ptout, phout, kjpk_in, kjpk_out, kn_var)      
      !!----------------------------------------------------------------------
      !!                    ***  ROUTINE  reconstructandremap ***
      !!
      !! ** Purpose :   Brief description of the routine
      !!
      !! ** Method  :   description of the methodoloy used to achieve the
      !!                objectives of the routine. Be as clear as possible!
      !!
      !! ** Action  : - first action (share memory array/varible modified
      !!                in this routine
      !!              - second action .....
      !!              - .....
      !!
      !! References :   Author et al., Short_name_review, Year
      !!                Give references if exist otherwise suppress these lines
      !!-----------------------------------------------------------------------
      INTEGER , INTENT(in   )                      ::   kjpk_in    ! Number of input levels
      INTEGER , INTENT(in   )                      ::   kjpk_out   ! Number of output levels
      INTEGER , INTENT(in   )                      ::   kn_var     ! Number of variables
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_in)  ::   phin       ! Input thicknesses
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_out) ::   phout      ! Output thicknesses
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_in , kn_var) ::   ptin       ! Input data
      REAL(wp), INTENT(inout), DIMENSION(kjpk_out, kn_var) ::   ptout      ! Remapped data
      !
      INTEGER             :: jk, jn, k1, kbox, ktop, ka, kbot
      REAL(wp), PARAMETER :: dpthin = 1.D-3, dsmll = 1.0D-8
      REAL(wp)            :: q, q01, q02, q001, q002, q0
      REAL(wp)            :: tsum, qbot, rpsum, zbox, ztop, zthk, zbot, offset, qtop
      REAL(wp)            :: coeffremap(kjpk_in,3), zwork(kjpk_in,3), zwork2(kjpk_in+1,3)
      REAL(wp)            :: z_win(1:kjpk_in+1), z_wout(1:kjpk_out+1)
      !!-----------------------------------------------------------------------

      z_win(1)=0._wp ; z_wout(1)= 0._wp
      DO jk = 1, kjpk_in
         z_win(jk+1)=z_win(jk)+phin(jk)
      END DO 
      
      DO jk = 1, kjpk_out
         z_wout(jk+1)=z_wout(jk)+phout(jk)       
      END DO       

      DO jk = 2, kjpk_in
         zwork(jk,1)=1._wp/(phin(jk-1)+phin(jk))
      END DO
        
      DO jk = 2, kjpk_in-1
         q0 = 1._wp / (phin(jk-1)+phin(jk)+phin(jk+1))
         zwork(jk,2) = phin(jk-1) + 2._wp*phin(jk) + phin(jk+1)
         zwork(jk,3) = q0
      END DO       
     
      DO jn = 1, kn_var

         DO jk = 2,kjpk_in
            zwork2(jk,1) = zwork(jk,1)*(ptin(jk,jn)-ptin(jk-1,jn))
         END DO
        
         coeffremap(:,1) = ptin(:,jn)
 
         DO jk = 2, kjpk_in-1
            q001 = phin(jk)*zwork2(jk+1,1)
            q002 = phin(jk)*zwork2(jk,1)        
            IF (q001*q002 < 0._wp) then
               q001 = 0._wp
               q002 = 0._wp
            ENDIF
            q=zwork(jk,2)
            q01=q*zwork2(jk+1,1)
            q02=q*zwork2(jk,1)
            IF (abs(q001) > abs(q02)) q001 = q02
            IF (abs(q002) > abs(q01)) q002 = q01
        
            q=(q001-q002)*zwork(jk,3)
            q001=q001-q*phin(jk+1)
            q002=q002+q*phin(jk-1)
        
            coeffremap(jk,3)=coeffremap(jk,1)+q001
            coeffremap(jk,2)=coeffremap(jk,1)-q002
        
            zwork2(jk,1)=(2._wp*q001-q002)**2
            zwork2(jk,2)=(2._wp*q002-q001)**2
         ENDDO
        
         DO jk = 1, kjpk_in
            IF(jk.EQ.1 .OR. jk.EQ.kjpk_in .OR. phin(jk).LE.dpthin) THEN
               coeffremap(jk,3) = coeffremap(jk,1)
               coeffremap(jk,2) = coeffremap(jk,1)
               zwork2(jk,1) = 0._wp
               zwork2(jk,2) = 0._wp
            ENDIF
         END DO
        
         DO jk = 2, kjpk_in
            q002 = max(zwork2(jk-1,2),dsmll)
            q001 = max(zwork2(jk,1)  ,dsmll)
            zwork2(jk,3) = (q001*coeffremap(jk-1,3)+q002*coeffremap(jk,2))/(q001+q002)
         END DO
        
         zwork2(1,3) = 2._wp*coeffremap(1,1)-zwork2(2,3)
         zwork2(kjpk_in+1,3)=2._wp*coeffremap(kjpk_in,1)-zwork2(kjpk_in,3)
 
         DO jk = 1, kjpk_in
            q01=zwork2(jk+1,3)-coeffremap(jk,1)
            q02=coeffremap(jk,1)-zwork2(jk,3)
            q001=2._wp*q01
            q002=2._wp*q02
            IF (q01*q02<0._wp) then
               q01=0._wp
               q02=0._wp
            ELSEIF (abs(q01)>abs(q002)) then
               q01=q002
            ELSEIF (abs(q02)>abs(q001)) then
               q02=q001
            ENDIF
            coeffremap(jk,2)=coeffremap(jk,1)-q02
            coeffremap(jk,3)=coeffremap(jk,1)+q01
         ENDDO

         zbot=0._wp
         kbot=1
         DO jk=1,kjpk_out
            ztop=zbot  !top is bottom of previous layer
            ktop=kbot
            IF     (ztop.GE.z_win(ktop+1)) then
               ktop=ktop+1
            ENDIF
        
            zbot=z_wout(jk+1)
            zthk=zbot-ztop

            IF(zthk.GT.dpthin .AND. ztop.LT.z_wout(kjpk_out+1)) THEN
 
               kbot=ktop
               DO while (z_win(kbot+1).lt.zbot.and.kbot.lt.kjpk_in)
                  kbot=kbot+1
               ENDDO
               zbox=zbot
               DO k1= jk+1,kjpk_out
                  IF     (z_wout(k1+1)-z_wout(k1).GT.dpthin) THEN
                     exit !thick layer
                  ELSE
                     zbox=z_wout(k1+1)  !include thin adjacent layers
                     IF(zbox.EQ.z_wout(kjpk_out+1)) THEN
                        exit !at bottom
                     ENDIF
                  ENDIF
               ENDDO
               zthk=zbox-ztop

               kbox=ktop
               DO while (z_win(kbox+1).lt.zbox.and.kbox.lt.kjpk_in)
                  kbox=kbox+1
               ENDDO
          
               IF(ktop.EQ.kbox) THEN
                  IF(z_wout(jk).NE.z_win(kbox).OR.z_wout(jk+1).NE.z_win(kbox+1)) THEN
                     IF(phin(kbox).GT.dpthin) THEN
                        q001 = (zbox-z_win(kbox))/phin(kbox)
                        q002 = (ztop-z_win(kbox))/phin(kbox)
                        q01=q001**2+q002**2+q001*q002+1._wp-2._wp*(q001+q002)
                        q02=q01-1._wp+(q001+q002)
                        q0=1._wp-q01-q02
                     ELSE
                        q0  = 1._wp
                        q01 = 0._wp
                        q02 = 0._wp
                     ENDIF
                     ptout(jk,jn)=q0*coeffremap(kbox,1)+q01*coeffremap(kbox,2)+q02*coeffremap(kbox,3)
                  ELSE
                     ptout(jk,jn) = ptin(kbox,jn)
                  ENDIF 
               ELSE
                  IF(ktop.LE.jk .AND. kbox.GE.jk) THEN
                     ka = jk
                  ELSEIF (kbox-ktop.GE.3) THEN
                     ka = (kbox+ktop)/2
                  ELSEIF (phin(ktop).GE.phin(kbox)) THEN
                     ka = ktop
                  ELSE
                     ka = kbox
                  ENDIF !choose ka
    
                  offset=coeffremap(ka,1)
    
                  qtop = z_win(ktop+1)-ztop !partial layer thickness
                  IF(phin(ktop).GT.dpthin) THEN
                     q=(ztop-z_win(ktop))/phin(ktop)
                     q01=q*(q-1._wp)
                     q02=q01+q
                     q0=1._wp-q01-q02            
                  ELSE
                     q0  = 1._wp
                     q01 = 0._wp
                     q02 = 0._wp
                  ENDIF
               
                  tsum =((q0*coeffremap(ktop,1)+q01*coeffremap(ktop,2)+q02*coeffremap(ktop,3))-offset)*qtop
    
                  DO k1= ktop+1,kbox-1
                     tsum =tsum +(coeffremap(k1,1)-offset)*phin(k1)
                  ENDDO !k1
               
                  qbot = zbox-z_win(kbox) !partial layer thickness
                  IF(phin(kbox).GT.dpthin) THEN
                     q=qbot/phin(kbox)
                     q01=(q-1._wp)**2
                     q02=q01-1._wp+q
                     q0=1_wp-q01-q02                            
                  ELSE
                     q0  = 1._wp
                     q01 = 0._wp
                     q02 = 0._wp
                  ENDIF
              
                  tsum = tsum +((q0*coeffremap(kbox,1)+q01*coeffremap(kbox,2)+q02*coeffremap(kbox,3))-offset)*qbot
               
                  rpsum=1._wp / zthk
                  ptout(jk,jn)=offset+tsum*rpsum
                 
               ENDIF !single or multiple layers
            ELSE
               IF (jk==1) THEN
                  write(*,'(a7,i4,i4,3f12.5)')'problem = ',kjpk_in,kjpk_out,zthk,z_wout(jk+1),phout(1)
               ENDIF
               ptout(jk,jn) = ptout(jk-1,jn)
             
            ENDIF !normal:thin layer
         ENDDO !jk

      END DO ! loop over variables
            
   END SUBROUTINE reconstructandremap

#else

   SUBROUTINE reconstructandremap(ptin, phin, ptout, phout, kjpk_in, kjpk_out, kn_var)
      !!----------------------------------------------------------------------
      !!                    *** ROUTINE  reconstructandremap ***
      !!
      !! ** Purpose :   Conservative remapping of a vertical column 
      !!                from one set of layers to an other one.
      !!
      !! ** Method  :   Uses D. Engwirda Piecewise Polynomial Reconstruction library.
      !!                https://github.com/dengwirda/PPR
      !!                
      !!
      !! References :   Engwirda, Darren & Kelley, Maxwell. (2015). A WENO-type 
      !!                slope-limiter for a family of piecewise polynomial methods. 
      !!                https://arxiv.org/abs/1606.08188
      !!-----------------------------------------------------------------------
      INTEGER , INTENT(in   )                      ::   kjpk_in    ! Number of input levels
      INTEGER , INTENT(in   )                      ::   kjpk_out   ! Number of output levels
      INTEGER , INTENT(in   )                      ::   kn_var     ! Number of variables
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_in)  ::   phin       ! Input thicknesses
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_out) ::   phout      ! Output thicknesses
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_in , kn_var) ::   ptin       ! Input data
      REAL(wp), INTENT(inout), DIMENSION(kjpk_out, kn_var) ::   ptout      ! Remapped data
      !
      INTEGER, PARAMETER :: ndof = 1
      INTEGER  :: jk, jn
      REAL(wp) ::  zwin(kjpk_in+1) ,  ztin(ndof, kn_var, kjpk_in)
      REAL(wp) :: zwout(kjpk_out+1), ztout(ndof, kn_var, kjpk_out)
      TYPE(rmap_work) :: work
      TYPE(rmap_opts) :: opts
      TYPE(rcon_ends) :: bc_l(kn_var)
      TYPE(rcon_ends) :: bc_r(kn_var)
      !!--------------------------------------------------------------------
     
      ! Set interfaces and input data:
      zwin(1) = 0._wp
      DO jk = 2, kjpk_in + 1
         zwin(jk) = zwin(jk-1) + phin(jk-1) 
      END DO
      
      DO jn = 1, kn_var 
         DO jk = 1, kjpk_in
            ztin(ndof, jn, jk) =  ptin(jk, jn)
         END DO
      END DO

      zwout(1) = 0._wp
      DO jk = 2, kjpk_out + 1
         zwout(jk) = zwout(jk-1) + phout(jk-1) 
      END DO

      ! specify methods
!      opts%edge_meth = p1e_method     ! 1st-order edge interp.
!      opts%cell_meth = pcm_method
!      opts%cell_meth = plm_method     ! PLM method in cells
      opts%edge_meth = p3e_method     ! 3rd-order edge interp.
      opts%cell_meth = ppm_method     ! PPM method in cells    
!      opts%edge_meth = p5e_method     ! 5th-order edge interp.
!      opts%cell_meth = pqm_method     ! PQM method in cells

      ! limiter
!      opts%cell_lims = null_limit     ! no lim.
!      opts%cell_lims = weno_limit
      opts%cell_lims = mono_limit     ! monotone limiter   
 
      ! set boundary conditions
      bc_l%bcopt = bcon_loose         ! "loose" = extrapolate
      bc_r%bcopt = bcon_loose
!      bc_l%bcopt = bcon_slope        
!      bc_r%bcopt = bcon_slope

      ! init. method workspace
      CALL work%init(kjpk_in+1, kn_var, opts)

      ! remap
      CALL rmap1d(kjpk_in+1, kjpk_out+1, kn_var, ndof, &
      &           zwin, zwout, ztin, ztout,            &
      &           bc_l, bc_r, work, opts)

      ! clear method workspace
      CALL work%free()

      DO jn = 1, kn_var 
         DO jk = 1, kjpk_out
            ptout(jk, jn) = ztout(1, jn, jk)
         END DO
      END DO
            
   END SUBROUTINE reconstructandremap
#endif

   SUBROUTINE remap_linear(ptin, pzin, ptout, pzout, kjpk_in, kjpk_out, kn_var)
      !!----------------------------------------------------------------------
      !!                    *** ROUTINE  remap_linear ***
      !!
      !! ** Purpose :   Linear interpolation based on input/ouputs depths
      !!
      !!-----------------------------------------------------------------------
      INTEGER , INTENT(in   )                      ::   kjpk_in    ! Number of input levels
      INTEGER , INTENT(in   )                      ::   kjpk_out   ! Number of output levels
      INTEGER , INTENT(in   )                      ::   kn_var     ! Number of variables
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_in)  ::   pzin       ! Input depths
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_out) ::   pzout      ! Output depths
      REAL(wp), INTENT(in   ), DIMENSION(kjpk_in , kn_var) ::   ptin       ! Input data
      REAL(wp), INTENT(inout), DIMENSION(kjpk_out, kn_var) ::   ptout      ! Interpolated data
      !
      INTEGER  :: jkin, jkout, jn
      !!--------------------------------------------------------------------
      !      
      DO jkout = 1, kjpk_out !  Loop over destination grid
         !
         IF     ( pzout(jkout) <=  pzin(  1    ) ) THEN ! Surface extrapolation	
            DO jn = 1, kn_var 
! linear
!               ptout(jkout,jn) = ptin(1 ,jn) + &
!                               & (pzout(jkout) - pzin(1)) / (pzin(2)    - pzin(1)) &
!                               &                          * (ptin(2,jn) - ptin(1,jn))
               ptout(jkout,jn) = ptin(1,jn)
            END DO
         ELSEIF ( pzout(jkout) >= pzin(kjpk_in) ) THEN ! Bottom extrapolation 
            DO jn = 1, kn_var 
! linear
!               ptout(jkout,jn) = ptin(kjpk_in ,jn) + &
!                               & (pzout(jkout) - pzin(kjpk_in)) / (pzin(kjpk_in)    - pzin(kjpk_in-1)) &
!                               &                                * (ptin(kjpk_in,jn) - ptin(kjpk_in-1,jn))
               ptout(jkout,jn) = ptin(kjpk_in ,jn)
            END DO
         ELSEIF ( ( pzout(jkout) > pzin(1) ).AND.( pzout(jkout) < pzin(kjpk_in) )) THEN
            DO jkin = 1, kjpk_in - 1 !  Loop over source grid
               IF ( pzout(jkout) < pzin(jkin+1) ) THEN
                  DO jn = 1, kn_var
                     ptout(jkout,jn) =  ptin(jkin,jn) + &
                                     & (pzout(jkout) - pzin(jkin)) / (pzin(jkin+1)    - pzin(jkin)) &
                                     &                             * (ptin(jkin+1,jn) - ptin(jkin,jn))
                  END DO  
                  EXIT
               ENDIF  
            END DO
         ENDIF
         !
      END DO

   END SUBROUTINE remap_linear

   !!======================================================================
!$AGRIF_END_DO_NOT_TREAT
END MODULE vremap
