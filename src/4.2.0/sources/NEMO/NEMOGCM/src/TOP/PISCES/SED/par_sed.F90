MODULE par_sed
   !!======================================================================
   !!                        ***  par_sed  ***
   !! Sediment :   set sediment parameter
   !!======================================================================
   !! History :
   !!        !  06-12  (C. Ethe)  Orignal
   !!----------------------------------------------------------------------
   !! $Id: par_sed.F90 15450 2021-10-27 14:32:08Z cetlod $

   !! Domain characteristics
   USE par_kind
   USE par_oce , ONLY :       &
      jpi      =>   jpi   ,  & !: first  dimension of grid --> i
      jpj      =>   jpj   ,  & !: second dimension of grid --> j
      jpij     =>   jpij  ,  & !: jpi x jpj
      jp_tem   =>   jp_tem,  & !: indice of temperature
      jp_sal   =>   jp_sal     !: indice of salintity

   INTEGER, PARAMETER :: jpdta = 18

   ! Vertical sediment geometry
   INTEGER, PUBLIC   ::      &
      jpksed   = 11 ,        &
      jpksedm1 = 10

   ! sediment tracer species
   INTEGER, PARAMETER ::    &
      jpsol =  8,           &  !: number of solid component
      jpwat = 11,           &   !: number of pore water component
      jpads = 2 ,           &   !! number adsorbed species
      jpwatp1 = jpwat +1,   &
      jpsol1 = jpsol - 1

   
   ! pore water components       
   INTEGER, PARAMETER :: &
      jwoxy  = 1,        & !: oxygen
      jwno3  = 2,        & !: nitrate
      jwpo4  = 3,        & !: phosphate
      jwnh4  = 4,        & !: Ammonium
      jwh2s  = 5,        & !: Sulfate
      jwso4  = 6,        & !: H2S
      jwfe2  = 7,        & !: Fe2+
      jwalk  = 8,        & !: Alkalinity
      jwlgw  = 9,        & !: Alkalinity
      jwdic  = 10,       & !: DIC
      jwsil  = 11          !: Silicate

   ! solid components       
   INTEGER, PARAMETER ::  &
      jsfeo   = 1,        & !: iron hydroxides
      jsfes   = 2,        & !: FeS
      jspoc   = 3,        & !: organic carbon
      jspos   = 4,        & !: semi-ref POC
      jspor   = 5,        & !: refractory POC
      jscal   = 6,        & !: Calcite
      jsopal  = 7,        & !: Opal
      jsclay  = 8           !: clay

   INTEGER, PARAMETER ::  &
      jptrased   = jpsol + jpwat , &
      jpvode     = jptrased - 11  , &
      jpdia3dsed = 3             , &
      jpdia2dsed = 22 

END MODULE par_sed
