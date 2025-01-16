PROGRAM test_connectivity_expand

  USE xios
  USE mod_wait
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: rank
  INTEGER :: size
  INTEGER :: ierr

  CHARACTER(len=*),PARAMETER :: id="client"
  INTEGER :: comm
  TYPE(xios_duration) :: dtime
  CHARACTER(len=20) :: dtime_str
  TYPE(xios_date) :: date
  CHARACTER(len=20) :: date_str
  CHARACTER(len=15) :: calendar_type
  TYPE(xios_context) :: ctx_hdl
  INTEGER,PARAMETER :: ni_glo=100
  INTEGER,PARAMETER :: nj_glo=100
  INTEGER,PARAMETER :: llm=5
  DOUBLE PRECISION  :: lval(llm)=1
  TYPE(xios_field) :: field_hdl
  TYPE(xios_fieldgroup) :: fieldgroup_hdl
  TYPE(xios_file) :: file_hdl
  LOGICAL :: ok

  DOUBLE PRECISION,DIMENSION(ni_glo,nj_glo) :: lon_glo,lat_glo
  DOUBLE PRECISION :: field_A_glo(ni_glo,nj_glo,llm)
  DOUBLE PRECISION,ALLOCATABLE :: lon(:,:),lat(:,:),field_A(:,:,:), lonvalue(:,:) ;
  INTEGER :: ni,ibegin,iend,nj,jbegin,jend
  INTEGER :: i,j,l,ts,n

!!! MPI Initialization
  CALL MPI_INIT(ierr)
  CALL init_wait

!!! XIOS Initialization (get the local communicator)
  CALL xios_initialize(id,return_comm=comm)

  CALL MPI_COMM_RANK(comm,rank,ierr)
  CALL MPI_COMM_SIZE(comm,size,ierr)

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
  DO n=0,size-1
    nj=nj_glo/size
    IF (n<MOD(nj_glo,size)) nj=nj+1
    IF (n==rank) exit
    jbegin=jbegin+nj
  ENDDO

  iend=ibegin+ni-1 ; jend=jbegin+nj-1

  ALLOCATE(lon(ni,nj),lat(ni,nj),field_A(0:ni+1,-1:nj+2,llm),lonvalue(ni,nj))
  lon(:,:)=lon_glo(ibegin+1:iend+1,jbegin+1:jend+1)
  lat(:,:)=lat_glo(ibegin+1:iend+1,jbegin+1:jend+1)
  field_A(1:ni,1:nj,:)=field_A_glo(ibegin+1:iend+1,jbegin+1:jend+1,:)

  CALL xios_context_initialize("test",comm)
  CALL xios_get_handle("test",ctx_hdl)
  CALL xios_set_current_context(ctx_hdl)

  CALL xios_get_calendar_type(calendar_type)
  PRINT *, "calendar_type = ", calendar_type

  CALL xios_set_axis_attr("axis_A",n_glo=llm ,value=lval) ;
  CALL xios_set_domain_attr("domain_A",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, ni=ni,jbegin=jbegin,nj=nj,type='curvilinear')
  CALL xios_set_domain_attr("domain_A",data_dim=2, data_ibegin=-1, data_ni=ni+2, data_jbegin=-2, data_nj=nj+4)
  CALL xios_set_domain_attr("domain_A",lonvalue_2D=lon,latvalue_2D=lat)
  CALL xios_set_fieldgroup_attr("field_definition",enabled=.TRUE.)

  dtime%second = 3600
  CALL xios_set_timestep(dtime)

  CALL xios_close_context_definition()

  CALL xios_get_domain_attr("domain_A",ni=ni,ibegin=ibegin, nj=nj, jbegin=jbegin)
  PRINT*, "domain_A attributes are ni = ",ni, ", ibegin = ", ibegin, ", nj= ", nj, ",jbegin = ", jbegin 
  CALL xios_get_domain_attr("domain_B",ni=ni,ibegin=ibegin, nj=nj, jbegin=jbegin)
  PRINT*, "domain_B attributes are ni = ",ni, ", ibegin = ", ibegin, ", nj= ", nj, ",jbegin = ", jbegin 
  
  DO ts=1,24
    CALL xios_update_calendar(ts)
    CALL xios_send_field("field_A",field_A)
    CALL wait_us(5000) ;
  ENDDO

  CALL xios_context_finalize()

  DEALLOCATE(lon, lat, field_A, lonvalue)

  CALL MPI_COMM_FREE(comm, ierr)

  CALL xios_finalize()

  CALL MPI_FINALIZE(ierr)

END PROGRAM test_connectivity_expand





