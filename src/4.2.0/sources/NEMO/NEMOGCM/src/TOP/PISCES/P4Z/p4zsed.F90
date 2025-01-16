MODULE p4zsed
   !!======================================================================
   !!                         ***  MODULE p4sed  ***
   !! TOP :   PISCES Compute loss of organic matter in the sediments
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12 (C. Ethe, G. Madec)  F90
   !!             3.4  !  2011-06 (C. Ethe) USE of fldread
   !!             3.5  !  2012-07 (O. Aumont) improvment of river input of nutrients 
   !!----------------------------------------------------------------------
   !!   p4z_sed        :  Compute loss of organic matter in the sediments
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zlim          !  Co-limitations of differents nutrients
   USE p4zint          !  interpolation and computation of various fields
   USE sed             !  Sediment module
   USE iom             !  I/O manager
   USE prtctl          !  print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sed  
   PUBLIC   p4z_sed_init
   PUBLIC   p4z_sed_alloc
 
   REAL(wp), PUBLIC ::   nitrfix      !: Nitrogen fixation rate
   REAL(wp), PUBLIC ::   diazolight   !: Nitrogen fixation sensitivty to light
   REAL(wp), PUBLIC ::   concfediaz   !: Fe half-saturation Cste for diazotrophs

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: nitrpot    !: Nitrogen fixation 
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:  ) :: sdenit     !: Nitrate reduction in the sediments
   !
   REAL(wp), SAVE :: r1_rday          
   REAL(wp), SAVE :: sedsilfrac, sedcalfrac

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p4zsed.F90 15287 2021-09-24 11:11:02Z cetlod $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE p4z_sed( kt, knt, Kbb, Kmm, Krhs )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sed  ***
      !!
      !! ** Purpose :   Compute loss of organic matter in the sediments. This
      !!              is by no way a sediment model. The loss is simply 
      !!              computed to balance the inout from rivers and dust
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      !
      INTEGER, INTENT(in) ::   kt, knt ! ocean time step
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! time level indices
      INTEGER  ::  ji, jj, jk, ikt
      REAL(wp) ::  zrivalk, zrivsil, zrivno3
      REAL(wp) ::  zlim, zfact, zfactcal
      REAL(wp) ::  zo2, zno3, zflx, zpdenit, z1pdenit, zolimit
      REAL(wp) ::  zsiloss, zcaloss, zws3, zws4, zwsc, zdep
      REAL(wp) ::  zwstpoc, zwstpon, zwstpop
      REAL(wp) ::  ztrfer, ztrpo4s, ztrdp, zwdust, zmudia, ztemp
      REAL(wp) ::  xdiano3, xdianh4
      !
      CHARACTER (len=25) :: charout
      REAL(wp), DIMENSION(jpi,jpj    ) :: zdenit2d, zbureff, zwork
      REAL(wp), DIMENSION(jpi,jpj    ) :: zwsbio3, zwsbio4
      REAL(wp), DIMENSION(jpi,jpj    ) :: zsedcal, zsedsi, zsedc
      REAL(wp), DIMENSION(jpi,jpj,jpk) :: zsoufer, zlight
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) :: ztrpo4, ztrdop, zirondep, zpdep
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )  CALL timing_start('p4z_sed')
      !

      ! Allocate temporary workspace
      ALLOCATE( ztrpo4(jpi,jpj,jpk) )
      IF( ln_p5z )    ALLOCATE( ztrdop(jpi,jpj,jpk) )

      zdenit2d(:,:) = 0.e0
      zbureff (:,:) = 0.e0
      zwork   (:,:) = 0.e0
      zsedsi  (:,:) = 0.e0
      zsedcal (:,:) = 0.e0
      zsedc   (:,:) = 0.e0

      IF( .NOT.lk_sed ) THEN
         ! OA: Warning, the following part is necessary to avoid CFL problems above the sediments
         ! --------------------------------------------------------------------
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            ikt  = mbkt(ji,jj)
            zdep = e3t(ji,jj,ikt,Kmm) / xstep
            zwsbio4(ji,jj) = MIN( 0.99 * zdep, wsbio4(ji,jj,ikt) )
            zwsbio3(ji,jj) = MIN( 0.99 * zdep, wsbio3(ji,jj,ikt) )
         END_2D

         ! Computation of the sediment denitrification proportion: The metamodel from midlleburg (2006) is being used
         ! Computation of the fraction of organic matter that is permanently buried from Dunne's model
         ! -------------------------------------------------------
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
           IF( tmask(ji,jj,1) == 1 ) THEN
              ikt = mbkt(ji,jj)
              zflx = (  tr(ji,jj,ikt,jpgoc,Kbb) * zwsbio4(ji,jj)   &
                &     + tr(ji,jj,ikt,jppoc,Kbb) * zwsbio3(ji,jj) )  * 1E3 * 1E6 / 1E4
              zflx  = LOG10( MAX( 1E-3, zflx ) )
              zo2   = LOG10( MAX( 10. , tr(ji,jj,ikt,jpoxy,Kbb) * 1E6 ) )
              zno3  = LOG10( MAX( 1.  , tr(ji,jj,ikt,jpno3,Kbb) * 1E6 * rno3 ) )
              zdep  = LOG10( gdepw(ji,jj,ikt+1,Kmm) )
              zdenit2d(ji,jj) = -2.2567 - 1.185 * zflx - 0.221 * zflx**2 - 0.3995 * zno3 * zo2 + 1.25 * zno3    &
                &                + 0.4721 * zo2 - 0.0996 * zdep + 0.4256 * zflx * zo2
              zdenit2d(ji,jj) = 10.0**( zdenit2d(ji,jj) )
                !
              zflx = (  tr(ji,jj,ikt,jpgoc,Kbb) * zwsbio4(ji,jj)   &
                &     + tr(ji,jj,ikt,jppoc,Kbb) * zwsbio3(ji,jj) ) * 1E6
              zbureff(ji,jj) = 0.013 + 0.53 * zflx**2 / ( 7.0 + zflx )**2
           ENDIF
         END_2D
         !
      ENDIF

      ! This loss is scaled at each bottom grid cell for equilibrating the total budget of silica in the ocean.
      ! Thus, the amount of silica lost in the sediments equal the supply at the surface (dust+rivers)
      ! ------------------------------------------------------
      IF( .NOT.lk_sed )  zrivsil = 1._wp - sedsilfrac

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         ikt  = mbkt(ji,jj)
         zdep = xstep / e3t(ji,jj,ikt,Kmm) 
         zwsc = zwsbio4(ji,jj) * zdep
         zsiloss = tr(ji,jj,ikt,jpgsi,Kbb) * zwsc
         zcaloss = tr(ji,jj,ikt,jpcal,Kbb) * zwsc
         !
         tr(ji,jj,ikt,jpgsi,Krhs) = tr(ji,jj,ikt,jpgsi,Krhs) - zsiloss
         tr(ji,jj,ikt,jpcal,Krhs) = tr(ji,jj,ikt,jpcal,Krhs) - zcaloss
      END_2D
      !
      IF( .NOT.lk_sed ) THEN
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            ikt  = mbkt(ji,jj)
            zdep = xstep / e3t(ji,jj,ikt,Kmm) 
            zwsc = zwsbio4(ji,jj) * zdep
            zsiloss = tr(ji,jj,ikt,jpgsi,Kbb) * zwsc
            zcaloss = tr(ji,jj,ikt,jpcal,Kbb) * zwsc
            tr(ji,jj,ikt,jpsil,Krhs) = tr(ji,jj,ikt,jpsil,Krhs) + zsiloss * zrivsil 
            !
            zfactcal = MIN( excess(ji,jj,ikt), 0.2 )
            zfactcal = MIN( 1., 1.3 * ( 0.2 - zfactcal ) / ( 0.4 - zfactcal ) )
            zrivalk  = sedcalfrac * zfactcal
            tr(ji,jj,ikt,jptal,Krhs) =  tr(ji,jj,ikt,jptal,Krhs) + zcaloss * zrivalk * 2.0
            tr(ji,jj,ikt,jpdic,Krhs) =  tr(ji,jj,ikt,jpdic,Krhs) + zcaloss * zrivalk
            zsedcal(ji,jj) = (1.0 - zrivalk) * zcaloss * e3t(ji,jj,ikt,Kmm) 
            zsedsi (ji,jj) = (1.0 - zrivsil) * zsiloss * e3t(ji,jj,ikt,Kmm) 
         END_2D
      ENDIF
      !
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
         ikt  = mbkt(ji,jj)
         zdep = xstep / e3t(ji,jj,ikt,Kmm) 
         zws4 = zwsbio4(ji,jj) * zdep
         zws3 = zwsbio3(ji,jj) * zdep
         tr(ji,jj,ikt,jpgoc,Krhs) = tr(ji,jj,ikt,jpgoc,Krhs) - tr(ji,jj,ikt,jpgoc,Kbb) * zws4 
         tr(ji,jj,ikt,jppoc,Krhs) = tr(ji,jj,ikt,jppoc,Krhs) - tr(ji,jj,ikt,jppoc,Kbb) * zws3
         tr(ji,jj,ikt,jpbfe,Krhs) = tr(ji,jj,ikt,jpbfe,Krhs) - tr(ji,jj,ikt,jpbfe,Kbb) * zws4
         tr(ji,jj,ikt,jpsfe,Krhs) = tr(ji,jj,ikt,jpsfe,Krhs) - tr(ji,jj,ikt,jpsfe,Kbb) * zws3
      END_2D
      !
      IF( ln_p5z ) THEN
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            ikt  = mbkt(ji,jj)
            zdep = xstep / e3t(ji,jj,ikt,Kmm) 
            zws4 = zwsbio4(ji,jj) * zdep
            zws3 = zwsbio3(ji,jj) * zdep
            tr(ji,jj,ikt,jpgon,Krhs) = tr(ji,jj,ikt,jpgon,Krhs) - tr(ji,jj,ikt,jpgon,Kbb) * zws4
            tr(ji,jj,ikt,jppon,Krhs) = tr(ji,jj,ikt,jppon,Krhs) - tr(ji,jj,ikt,jppon,Kbb) * zws3
            tr(ji,jj,ikt,jpgop,Krhs) = tr(ji,jj,ikt,jpgop,Krhs) - tr(ji,jj,ikt,jpgop,Kbb) * zws4
            tr(ji,jj,ikt,jppop,Krhs) = tr(ji,jj,ikt,jppop,Krhs) - tr(ji,jj,ikt,jppop,Kbb) * zws3
         END_2D
      ENDIF

      IF( .NOT.lk_sed ) THEN
         ! The 0.5 factor in zpdenit is to avoid negative NO3 concentration after
         ! denitrification in the sediments. Not very clever, but simpliest option.
         DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            ikt  = mbkt(ji,jj)
            zdep = xstep / e3t(ji,jj,ikt,Kmm) 
            zws4 = zwsbio4(ji,jj) * zdep
            zws3 = zwsbio3(ji,jj) * zdep
            zrivno3 = 1. - zbureff(ji,jj)
            zwstpoc = tr(ji,jj,ikt,jpgoc,Kbb) * zws4 + tr(ji,jj,ikt,jppoc,Kbb) * zws3
            zpdenit  = MIN( 0.5 * ( tr(ji,jj,ikt,jpno3,Kbb) - rtrn ) / rdenit, zdenit2d(ji,jj) * zwstpoc * zrivno3 )
            z1pdenit = zwstpoc * zrivno3 - zpdenit
            zolimit = MIN( ( tr(ji,jj,ikt,jpoxy,Kbb) - rtrn ) / o2ut, z1pdenit * ( 1.- nitrfac(ji,jj,ikt) ) )
            tr(ji,jj,ikt,jpdoc,Krhs) = tr(ji,jj,ikt,jpdoc,Krhs) + z1pdenit - zolimit
            tr(ji,jj,ikt,jppo4,Krhs) = tr(ji,jj,ikt,jppo4,Krhs) + zpdenit + zolimit
            tr(ji,jj,ikt,jpnh4,Krhs) = tr(ji,jj,ikt,jpnh4,Krhs) + zpdenit + zolimit
            tr(ji,jj,ikt,jpno3,Krhs) = tr(ji,jj,ikt,jpno3,Krhs) - rdenit * zpdenit
            tr(ji,jj,ikt,jpoxy,Krhs) = tr(ji,jj,ikt,jpoxy,Krhs) - zolimit * o2ut
            tr(ji,jj,ikt,jptal,Krhs) = tr(ji,jj,ikt,jptal,Krhs) + rno3 * (zolimit + (1.+rdenit) * zpdenit )
            tr(ji,jj,ikt,jpdic,Krhs) = tr(ji,jj,ikt,jpdic,Krhs) + zpdenit + zolimit 
            sdenit(ji,jj) = rdenit * zpdenit * e3t(ji,jj,ikt,Kmm)
            zsedc(ji,jj)   = (1. - zrivno3) * zwstpoc * e3t(ji,jj,ikt,Kmm)
            IF( ln_p5z ) THEN
               zwstpop              = tr(ji,jj,ikt,jpgop,Kbb) * zws4 + tr(ji,jj,ikt,jppop,Kbb) * zws3
               zwstpon              = tr(ji,jj,ikt,jpgon,Kbb) * zws4 + tr(ji,jj,ikt,jppon,Kbb) * zws3
               tr(ji,jj,ikt,jpdon,Krhs) = tr(ji,jj,ikt,jpdon,Krhs) + ( z1pdenit - zolimit ) * zwstpon / (zwstpoc + rtrn)
               tr(ji,jj,ikt,jpdop,Krhs) = tr(ji,jj,ikt,jpdop,Krhs) + ( z1pdenit - zolimit ) * zwstpop / (zwstpoc + rtrn)
            ENDIF
         END_2D
       ENDIF


      ! Nitrogen fixation process
      ! Small source iron from particulate inorganic iron
      !-----------------------------------
      DO jk = 1, jpkm1
         zlight (:,:,jk) =  ( 1.- EXP( -etot_ndcy(:,:,jk) / diazolight ) ) * ( 1. - fr_i(:,:) ) 
         zsoufer(:,:,jk) = zlight(:,:,jk) * 2E-11 / ( 2E-11 + biron(:,:,jk) )
      ENDDO
      IF( ln_p4z ) THEN
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            !                      ! Potential nitrogen fixation dependant on temperature and iron
            ztemp = ts(ji,jj,jk,jp_tem,Kmm)
            zmudia = MAX( 0.,-0.001096*ztemp**2 + 0.057*ztemp -0.637 ) * 7.625
            !       Potential nitrogen fixation dependant on temperature and iron
            xdianh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( concnnh4 + tr(ji,jj,jk,jpnh4,Kbb) )
            xdiano3 = tr(ji,jj,jk,jpno3,Kbb) / ( concnno3 + tr(ji,jj,jk,jpno3,Kbb) ) * (1. - xdianh4)
            zlim = ( 1.- xdiano3 - xdianh4 )
            IF( zlim <= 0.1 )   zlim = 0.01
            zfact = zlim * rfact2
            ztrfer = biron(ji,jj,jk) / ( concfediaz + biron(ji,jj,jk) )
            ztrpo4(ji,jj,jk) = tr(ji,jj,jk,jppo4,Kbb) / ( 1E-6 + tr(ji,jj,jk,jppo4,Kbb) )
            ztrdp = ztrpo4(ji,jj,jk)
            nitrpot(ji,jj,jk) =  zmudia * r1_rday * zfact * MIN( ztrfer, ztrdp ) * zlight(ji,jj,jk)
         END_3D
      ELSE       ! p5z
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            !                      ! Potential nitrogen fixation dependant on temperature and iron
            ztemp = ts(ji,jj,jk,jp_tem,Kmm)
            zmudia = MAX( 0.,-0.001096*ztemp**2 + 0.057*ztemp -0.637 ) * 7.625
            !       Potential nitrogen fixation dependant on temperature and iron
            xdianh4 = tr(ji,jj,jk,jpnh4,Kbb) / ( concnnh4 + tr(ji,jj,jk,jpnh4,Kbb) )
            xdiano3 = tr(ji,jj,jk,jpno3,Kbb) / ( concnno3 + tr(ji,jj,jk,jpno3,Kbb) ) * (1. - xdianh4)
            zlim = ( 1.- xdiano3 - xdianh4 )
            IF( zlim <= 0.1 )   zlim = 0.01
            zfact = zlim * rfact2
            ztrfer = biron(ji,jj,jk) / ( concfediaz + biron(ji,jj,jk) )
            ztrpo4(ji,jj,jk) = tr(ji,jj,jk,jppo4,Kbb) / ( 1E-6 + tr(ji,jj,jk,jppo4,Kbb) )
            ztrdop(ji,jj,jk) = tr(ji,jj,jk,jpdop,Kbb) / ( 1E-6 + tr(ji,jj,jk,jpdop,Kbb) ) * (1. - ztrpo4(ji,jj,jk))
            ztrdp = ztrpo4(ji,jj,jk) + ztrdop(ji,jj,jk)
            nitrpot(ji,jj,jk) =  zmudia * r1_rday * zfact * MIN( ztrfer, ztrdp ) * zlight(ji,jj,jk)
         END_3D
      ENDIF

      ! Nitrogen change due to nitrogen fixation
      ! ----------------------------------------
      IF( ln_p4z ) THEN
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            zfact = nitrpot(ji,jj,jk) * nitrfix
            tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zfact / 3.0
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * zfact / 3.0
            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zfact * 2.0 / 3.0            
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - zfact * 2.0 / 3.0
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zfact * 1.0 / 3.0
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zfact * 1.0 / 3.0 * 2.0 / 3.0
            tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) + zfact * 1.0 / 3.0 * 1.0 / 3.0
            tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) + ( o2ut + o2nit ) * zfact * 2.0 / 3.0 + o2nit * zfact / 3.0
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - 30E-6 * zfact * 1.0 / 3.0
            tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + 30E-6 * zfact * 1.0 / 3.0 * 2.0 / 3.0
            tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + 30E-6 * zfact * 1.0 / 3.0 * 1.0 / 3.0
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + 0.002 * 4E-10 * zsoufer(ji,jj,jk) * rfact2 / rday
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) + concdnh4 / ( concdnh4 + tr(ji,jj,jk,jppo4,Kbb) ) &
            &                     * 0.001 * tr(ji,jj,jk,jpdoc,Kbb) * xstep
         END_3D
      ELSE    ! p5z
         DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1)
            zfact = nitrpot(ji,jj,jk) * nitrfix
            tr(ji,jj,jk,jpnh4,Krhs) = tr(ji,jj,jk,jpnh4,Krhs) + zfact / 3.0
            tr(ji,jj,jk,jptal,Krhs) = tr(ji,jj,jk,jptal,Krhs) + rno3 * zfact / 3.0
            tr(ji,jj,jk,jpdic,Krhs) = tr(ji,jj,jk,jpdic,Krhs) - zfact * 2.0 / 3.0            
            tr(ji,jj,jk,jppo4,Krhs) = tr(ji,jj,jk,jppo4,Krhs) - 16.0 / 46.0 * zfact * ( 1.0 - 1.0 / 3.0 ) &
            &                     * ztrpo4(ji,jj,jk) / (ztrpo4(ji,jj,jk) + ztrdop(ji,jj,jk) + rtrn)
            tr(ji,jj,jk,jpdon,Krhs) = tr(ji,jj,jk,jpdon,Krhs) + zfact * 1.0 / 3.0
            tr(ji,jj,jk,jpdoc,Krhs) = tr(ji,jj,jk,jpdoc,Krhs) + zfact * 1.0 / 3.0
            tr(ji,jj,jk,jpdop,Krhs) = tr(ji,jj,jk,jpdop,Krhs) + 16.0 / 46.0 * zfact / 3.0  &
            &                     - 16.0 / 46.0 * zfact * ztrdop(ji,jj,jk)   &
            &                     / (ztrpo4(ji,jj,jk) + ztrdop(ji,jj,jk) + rtrn)
            tr(ji,jj,jk,jppoc,Krhs) = tr(ji,jj,jk,jppoc,Krhs) + zfact * 1.0 / 3.0 * 2.0 / 3.0
            tr(ji,jj,jk,jppon,Krhs) = tr(ji,jj,jk,jppon,Krhs) + zfact * 1.0 / 3.0 * 2.0 /3.0
            tr(ji,jj,jk,jppop,Krhs) = tr(ji,jj,jk,jppop,Krhs) + 16.0 / 46.0 * zfact * 1.0 / 3.0 * 2.0 /3.0
            tr(ji,jj,jk,jpgoc,Krhs) = tr(ji,jj,jk,jpgoc,Krhs) + zfact * 1.0 / 3.0 * 1.0 / 3.0
            tr(ji,jj,jk,jpgon,Krhs) = tr(ji,jj,jk,jpgon,Krhs) + zfact * 1.0 / 3.0 * 1.0 /3.0
            tr(ji,jj,jk,jpgop,Krhs) = tr(ji,jj,jk,jpgop,Krhs) + 16.0 / 46.0 * zfact * 1.0 / 3.0 * 1.0 /3.0
            tr(ji,jj,jk,jpoxy,Krhs) = tr(ji,jj,jk,jpoxy,Krhs) + ( o2ut + o2nit ) * zfact * 2.0 / 3.0 + o2nit * zfact / 3.0
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) - 30E-6 * zfact * 1.0 / 3.0 
            tr(ji,jj,jk,jpsfe,Krhs) = tr(ji,jj,jk,jpsfe,Krhs) + 30E-6 * zfact * 1.0 / 3.0 * 2.0 / 3.0
            tr(ji,jj,jk,jpbfe,Krhs) = tr(ji,jj,jk,jpbfe,Krhs) + 30E-6 * zfact * 1.0 / 3.0 * 1.0 / 3.0
            tr(ji,jj,jk,jpfer,Krhs) = tr(ji,jj,jk,jpfer,Krhs) + 0.002 * 4E-10 * zsoufer(ji,jj,jk) * rfact2 / rday
         END_3D
         !
      ENDIF

      IF( lk_iomput .AND. knt == nrdttrc ) THEN
         zfact = 1.e+3 * rfact2r !  conversion from molC/l/kt  to molN/m3/s
         CALL iom_put( "Nfix", nitrpot(:,:,:) * nitrfix * rno3 * zfact * tmask(:,:,:) )  ! nitrogen fixation 
         CALL iom_put( "SedCal", zsedcal(:,:) * zfact )
         CALL iom_put( "SedSi" , zsedsi (:,:) * zfact )
         CALL iom_put( "SedC"  , zsedc  (:,:) * zfact )
         CALL iom_put( "Sdenit", sdenit (:,:) * zfact * rno3 )
      ENDIF
      !
      IF(sn_cfctl%l_prttrc) THEN  ! print mean trneds (USEd for debugging)
         WRITE(charout, fmt="('sed ')")
         CALL prt_ctl_info( charout, cdcomp = 'top' )
         CALL prt_ctl(tab4d_1=tr(:,:,:,:,Krhs), mask1=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_p5z )    DEALLOCATE( ztrpo4, ztrdop )
      !
      IF( ln_timing )  CALL timing_stop('p4z_sed')
      !
   END SUBROUTINE p4z_sed

   SUBROUTINE p4z_sed_init
      !!----------------------------------------------------------------------
      !!                  ***  routine p4z_sed_init  ***
      !!
      !! ** purpose :   initialization of some parameters
      !!
      !!----------------------------------------------------------------------
      !!----------------------------------------------------------------------
      INTEGER  :: ji, jj, jk, jm
      INTEGER  :: ios                 ! Local integer output status for namelist read
      !
      !!
      NAMELIST/nampissed/ nitrfix, diazolight, concfediaz
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'p4z_sed_init : initialization of sediment mobilisation '
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      !                            !* set file information
      READ  ( numnatp_ref, nampissed, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nampissed in reference namelist' )
      READ  ( numnatp_cfg, nampissed, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'nampissed in configuration namelist' )
      IF(lwm) WRITE ( numonp, nampissed )

      IF(lwp) THEN
         WRITE(numout,*) '   Namelist : nampissed '
         WRITE(numout,*) '      nitrogen fixation rate                       nitrfix = ', nitrfix
         WRITE(numout,*) '      nitrogen fixation sensitivty to light    diazolight  = ', diazolight
         WRITE(numout,*) '      Fe half-saturation cste for diazotrophs  concfediaz  = ', concfediaz
      ENDIF
      !
      r1_rday  = 1. / rday
      !
      sedsilfrac = 0.03     ! percentage of silica loss in the sediments
      sedcalfrac = 0.6      ! percentage of calcite loss in the sediments
      !
      lk_sed = ln_sediment .AND. ln_sed_2way 
      !
      nitrpot(:,:,jpk) = 0._wp   ! define last level for iom_put
      !
   END SUBROUTINE p4z_sed_init

   INTEGER FUNCTION p4z_sed_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sed_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( nitrpot(jpi,jpj,jpk), sdenit(jpi,jpj), STAT=p4z_sed_alloc )
      !
      IF( p4z_sed_alloc /= 0 )   CALL ctl_stop( 'STOP', 'p4z_sed_alloc: failed to allocate arrays' )
      !
   END FUNCTION p4z_sed_alloc

   !!======================================================================
END MODULE p4zsed
