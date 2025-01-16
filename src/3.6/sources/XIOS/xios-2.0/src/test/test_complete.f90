PROGRAM test_complete

  USE xios
  USE mod_wait
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: rank
  INTEGER :: size_loc
  INTEGER :: ierr

  CHARACTER(len=*),PARAMETER :: id="client"
  INTEGER :: comm
  TYPE(xios_duration)  :: dtime
  TYPE(xios_context) :: ctx_hdl
  INTEGER,PARAMETER :: ni_glo=100
  INTEGER,PARAMETER :: nj_glo=100
  INTEGER,PARAMETER :: llm=5
  DOUBLE PRECISION  :: lval(llm)=1
  TYPE(xios_field) :: field_hdl
  TYPE(xios_fieldgroup) :: fieldgroup_hdl
  TYPE(xios_file) :: file_hdl
  LOGICAL :: ok
  CHARACTER(len=256) :: crname

  DOUBLE PRECISION,DIMENSION(ni_glo,nj_glo) :: lon_glo,lat_glo
  DOUBLE PRECISION :: field_A_glo(ni_glo,nj_glo,llm)
  DOUBLE PRECISION,ALLOCATABLE :: lon(:,:),lat(:,:),field_A_atm(:,:,:), field_A_srf(:,:), lonvalue(:,:)
  INTEGER, ALLOCATABLE :: kindex(:)
  INTEGER :: ni,ibegin,iend,nj,jbegin,jend
  INTEGER :: i,j,l,ts,n, nb_pt

!!! MPI Initialization

  CALL MPI_INIT(ierr)

  CALL init_wait

!!! XIOS Initialization (get the local communicator)

  CALL xios_initialize(id,return_comm=comm)

  CALL MPI_COMM_RANK(comm,rank,ierr)
  CALL MPI_COMM_SIZE(comm,size_loc,ierr)


!###########################################################################
! Contexte ATM
!###########################################################################

!!! Initialisation des coordonnées globales et locales pour la grille régulière

  DO j=1,nj_glo
    DO i=1,ni_glo
      lon_glo(i,j)=(i-1)+(j-1)*ni_glo
      lat_glo(i,j)=1000+(i-1)+(j-1)*ni_glo
      DO l=1,llm
        field_A_glo(i,j,l)=(i-1)+(j-1)*ni_glo+10000*l
      ENDDO
    ENDDO
  ENDDO
  ni=ni_glo ; ibegin=0

  jbegin=0
  DO n=0,size_loc-1
    nj=nj_glo/size_loc
    IF (n<MOD(nj_glo,size_loc)) nj=nj+1
    IF (n==rank) exit
    jbegin=jbegin+nj
  ENDDO

  iend=ibegin+ni-1 ; jend=jbegin+nj-1

  ALLOCATE(lon(ni,nj),lat(ni,nj),field_A_atm(0:ni+1,-1:nj+2,llm),lonvalue(ni,nj))
  lon(:,:)=lon_glo(ibegin+1:iend+1,jbegin+1:jend+1)
  lat(:,:)=lat_glo(ibegin+1:iend+1,jbegin+1:jend+1)
  field_A_atm(1:ni,1:nj,:)=field_A_glo(ibegin+1:iend+1,jbegin+1:jend+1,:)


!!! Context ATMOSPHERE

  CALL xios_context_initialize("atmosphere",comm)
  CALL xios_get_handle("atmosphere",ctx_hdl)
  CALL xios_set_current_context(ctx_hdl)

  CALL xios_define_calendar(type="Gregorian", &
                            start_date=xios_date(2000, 01, 01, 00, 00, 00), &
                            time_origin=xios_date(1999, 01, 01, 15, 00, 00))

  CALL xios_set_axis_attr("axis_atm",n_glo=llm ,value=lval) ;
!  CALL xios_set_zoom_axis_attr("axis_atm_zoom",n=2);

  CALL xios_set_domain_attr("domain_atm",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, ni=ni,jbegin=jbegin,nj=nj, type='curvilinear')
  CALL xios_set_domain_attr("domain_atm",data_dim=2, data_ibegin=-1, data_ni=ni+2, data_jbegin=-2, data_nj=nj+4)
  CALL xios_set_domain_attr("domain_atm",lonvalue_2D=lon,latvalue_2D=lat)

  CALL xios_set_domain_attr("domain_atm_zoom",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, ni=ni,jbegin=jbegin,nj=nj)
  CALL xios_set_domain_attr("domain_atm_zoom",data_dim=2, data_ibegin=-1, data_ni=ni+2, data_jbegin=-2, data_nj=nj+4)
  CALL xios_set_domain_attr("domain_atm_zoom",lonvalue_2D=lon,latvalue_2D=lat)
!  CALL xios_set_zoom_domain_attr("domain_atm_zoom",ibegin=39, ni=20, jbegin=39, nj=5)

!!! Activation du groupe field_definition

  CALL xios_set_fieldgroup_attr("field_definition",enabled=.TRUE.)

!!! Création d un nouveau champ

  CALL xios_get_handle("field_definition",fieldgroup_hdl)
  CALL xios_add_child(fieldgroup_hdl,field_hdl,"field_B_atm")

!!! Heritage des attributs d un autre champ

  CALL xios_set_attr(field_hdl,field_ref="field_A_atm_zoom",name="field_B_atm")

!!! Affectation de ce nouveau champ au fichier avec un nouveau nom

  CALL xios_get_handle("output_atmosphere",file_hdl)
  CALL xios_add_child(file_hdl,field_hdl)
  CALL xios_set_attr(field_hdl,field_ref="field_B_atm",name="field_C_atm")

!!! Definition du timestep

  dtime%second=3600
  CALL xios_set_timestep(timestep=dtime)

!!! Recupration des valeurs des longitudes et de taille des domaines locaux (pour test de fonctionnalité)

  ni=0 ; lonvalue(:,:)=0
  CALL xios_get_domain_attr("domain_atm",ni=ni,lonvalue_2D=lonvalue)

  PRINT *,"ni",ni
  PRINT *,"lonvalue",lonvalue;

!!! Fin de la definition du contexte

  CALL xios_close_context_definition()

!!! Test des valeurs des champs/fichiers

  !!! Attribut defini ?

  CALL xios_is_defined_field_attr("field_A_atm",enabled=ok)
  PRINT *,"field_A_atm : attribute enabled is defined ? ",ok

  !!! Recuperer la valeur d un attribut

  CALL xios_get_field_attr("field_A_atm",name=crname)
  PRINT *,"field_A_atm : attribute name is : ",TRIM(crname)

  !!! Champ actif (besoin de fournir la valeur) ?

    PRINT*,"field field_A_atm is active ? ",xios_field_is_active("field_A_atm")

  !!! Champ defini ?

    PRINT*,"field field_A_atm is valid ?",xios_is_valid_field("field_A_atm")


!###########################################################################
! Contexte SRF
!###########################################################################

!!! Initialisation des coordonnées globales et locales pour la grille indexee (1 point sur 2)

    nb_pt=ni*nj/2
    ALLOCATE(kindex(nb_pt),field_A_srf(nb_pt,llm))
    DO i=1,nb_pt
      kindex(i)=2*i-1
    ENDDO
    field_A_srf(1:nb_pt,:)=RESHAPE(field_A_glo(ibegin+1:iend+1:2,jbegin+1:jend+1,:),(/ nb_pt,llm /))

  CALL xios_context_initialize("surface",comm)
  CALL xios_get_handle("surface",ctx_hdl)
  CALL xios_set_current_context(ctx_hdl)

  CALL xios_define_calendar(type="Gregorian", &
                            start_date=xios_date(2000, 01, 01, 00, 00, 00), &
                            time_origin=xios_date(1999, 01, 01, 15, 00, 00))

  CALL xios_set_axis_attr("axis_srf",n_glo=llm ,value=lval)
  CALL xios_set_domain_attr("domain_srf",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, ni=ni,jbegin=jbegin,nj=nj, type='curvilinear')
  CALL xios_set_domain_attr("domain_srf",data_dim=1, data_ibegin=0, data_ni=nb_pt)
  CALL xios_set_domain_attr("domain_srf",data_i_index=kindex)
  CALL xios_set_domain_attr("domain_srf",lonvalue_2D=lon,latvalue_2D=lat)

!!! Création d un nouveau champ

  CALL xios_get_handle("field_definition",fieldgroup_hdl)
  CALL xios_add_child(fieldgroup_hdl,field_hdl,"field_B_srf")

!!! Heritage des attributs d un autre champ

  CALL xios_set_attr(field_hdl,field_ref="field_A_srf",name="field_B_srf")

!!! Affectation de ce nouveau champ au fichier avec un nouveau nom

  CALL xios_get_handle("output_surface",file_hdl)
  CALL xios_add_child(file_hdl,field_hdl)
  CALL xios_set_attr(field_hdl,field_ref="field_B_srf",name="field_C_srf")

!!! Definition du timestep

  dtime%second=1800
  CALL xios_set_timestep(timestep=dtime)

!!! Recupration des valeurs des longitudes et de taille des domaines locaux (pour test de fonctionnalité)

  ni=0 ; lonvalue(:,:)=0
  CALL xios_get_domain_attr("domain_srf",ni=ni,lonvalue_2D=lonvalue)

  PRINT *,"ni",ni
  PRINT *,"lonvalue",lonvalue ;

!!! Fin de la definition du contexte SRF

  CALL xios_close_context_definition()


!####################################################################################
!!! Boucle temporelle
!####################################################################################

    DO ts=1,24*10

      CALL xios_get_handle("atmosphere",ctx_hdl)
      CALL xios_set_current_context(ctx_hdl)

!!! Mise a jour du pas de temps

      CALL xios_update_calendar(ts)

!!! On donne la valeur du champ atm

      CALL xios_send_field("field_A_atm",field_A_atm)

!!! On change de contexte

      CALL xios_get_handle("surface",ctx_hdl)
      CALL xios_set_current_context(ctx_hdl)

!!! Mise a jour du pas de temps

      CALL xios_update_calendar(ts)

!!! On donne la valeur du champ srf

      CALL xios_send_field("field_A_srf",field_A_srf)

      CALL wait_us(5000) ;
    ENDDO

!####################################################################################
!!! Finalisation
!####################################################################################

!!! Fin des contextes

    CALL xios_context_finalize()
    CALL xios_get_handle("atmosphere",ctx_hdl)
    CALL xios_set_current_context(ctx_hdl)
    CALL xios_context_finalize()

    DEALLOCATE(lon, lat, field_A_atm, lonvalue)
    DEALLOCATE(kindex, field_A_srf)

!!! Fin de XIOS

    CALL MPI_COMM_FREE(comm, ierr)

    CALL xios_finalize()

    CALL MPI_FINALIZE(ierr)

  END PROGRAM test_complete






