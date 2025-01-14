MODULE logical_bool_conversion

  USE, INTRINSIC :: ISO_C_BINDING
  INTERFACE
    FUNCTION cxios_set_logical_true() BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_set_logical_true
    END FUNCTION cxios_set_logical_true

    FUNCTION cxios_set_logical_false() BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_set_logical_false
    END FUNCTION cxios_set_logical_false
  END INTERFACE

CONTAINS
  SUBROUTINE xios_logical_to_bool_0d(tmp)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp
    LOGICAL (KIND=C_BOOL) :: ctrue, cfalse
    ctrue  =  cxios_set_logical_true()
    cfalse =  cxios_set_logical_false()
    if (tmp.eqv..false.) then
      tmp = cfalse
    else
      tmp = ctrue
    endif
  END SUBROUTINE xios_logical_to_bool_0d

  SUBROUTINE xios_logical_to_bool_1d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: ij
    LOGICAL (KIND=C_BOOL) :: ctrue, cfalse
    ctrue  =  cxios_set_logical_true()
    cfalse =  cxios_set_logical_false()
    do ij=1,ni_(1)
       if (tmp(ij).eqv..false.) then
          tmp(ij) = cfalse
       else
          tmp(ij) = ctrue
       endif
    enddo
  END SUBROUTINE xios_logical_to_bool_1d

  SUBROUTINE xios_logical_to_bool_2d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j
    LOGICAL (KIND=C_BOOL) :: ctrue, cfalse
    ctrue  =  cxios_set_logical_true()
    cfalse =  cxios_set_logical_false()
    do j=1,ni_(2)
      do i=1,ni_(1)
        if (tmp(i,j).eqv..false.) then
          tmp(i,j) = cfalse
        else
          tmp(i,j) = ctrue
        endif
      enddo
    enddo
  END SUBROUTINE xios_logical_to_bool_2d

  SUBROUTINE xios_logical_to_bool_3d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k
    LOGICAL (KIND=C_BOOL) :: ctrue, cfalse
    ctrue  =  cxios_set_logical_true()
    cfalse =  cxios_set_logical_false()
    do k=1,ni_(3)
      do j=1,ni_(2)
        do i=1,ni_(1)
          if (tmp(i,j,k).eqv..false.) then
            tmp(i,j,k) = cfalse
          else
            tmp(i,j,k) = ctrue
          endif
        enddo
      enddo
    enddo
  END SUBROUTINE xios_logical_to_bool_3d
  
  SUBROUTINE xios_logical_to_bool_4d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k,l
    LOGICAL (KIND=C_BOOL) :: ctrue, cfalse
    ctrue  =  cxios_set_logical_true()
    cfalse =  cxios_set_logical_false()
    do l=1,ni_(4)
      do k=1,ni_(3)
        do j=1,ni_(2)
          do i=1,ni_(1)
            if (tmp(i,j,k,l).eqv..false.) then
              tmp(i,j,k,l) = cfalse
            else
              tmp(i,j,k,l) = ctrue
            endif
          enddo
        enddo
      enddo
    enddo
  END SUBROUTINE xios_logical_to_bool_4d

  SUBROUTINE xios_logical_to_bool_5d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k,l,m
    LOGICAL (KIND=C_BOOL) :: ctrue, cfalse
    ctrue  =  cxios_set_logical_true()
    cfalse =  cxios_set_logical_false()
    do m=1,ni_(5)
      do l=1,ni_(4)
        do k=1,ni_(3)
          do j=1,ni_(2)
            do i=1,ni_(1)
              if (tmp(i,j,k,l,m).eqv..false.) then
                tmp(i,j,k,l,m) = cfalse
              else
                tmp(i,j,k,l,m) = ctrue
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
  END SUBROUTINE xios_logical_to_bool_5d
  
  SUBROUTINE xios_logical_to_bool_6d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:,:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k,l,m,p
    LOGICAL (KIND=C_BOOL) :: ctrue, cfalse
    ctrue  =  cxios_set_logical_true()
    cfalse =  cxios_set_logical_false()
    do p=1,ni_(6)
      do m=1,ni_(5)
        do l=1,ni_(4)
          do k=1,ni_(3)
            do j=1,ni_(2)
              do i=1,ni_(1)
                if (tmp(i,j,k,l,m,p).eqv..false.) then
                  tmp(i,j,k,l,m,p) = cfalse
                else
                  tmp(i,j,k,l,m,p) = ctrue
                endif
              enddo
            enddo
          enddo
        enddo
      enddo
    enddo
  END SUBROUTINE xios_logical_to_bool_6d
  
  SUBROUTINE xios_logical_to_bool_7d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:,:,:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k,l,m,p,q
    LOGICAL (KIND=C_BOOL) :: ctrue, cfalse
    ctrue  =  cxios_set_logical_true()
    cfalse =  cxios_set_logical_false()
    do q=1,ni_(7)
      do p=1,ni_(6)
        do m=1,ni_(5)
          do l=1,ni_(4)
            do k=1,ni_(3)
              do j=1,ni_(2)
                do i=1,ni_(1)
                  if (tmp(i,j,k,l,m,p,q).eqv..false.) then
                    tmp(i,j,k,l,m,p,q) = cfalse
                  else
                    tmp(i,j,k,l,m,p,q) = ctrue
                  endif
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
    enddo
  END SUBROUTINE xios_logical_to_bool_7d


  SUBROUTINE xios_bool_to_logical_0d(tmp)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp
    if (tmp.eqv..false.) then
      tmp = .false.
    else
      tmp = .true.
    endif
  END SUBROUTINE xios_bool_to_logical_0d

  SUBROUTINE xios_bool_to_logical_1d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: ij
    do ij=1,ni_(1)
       if (tmp(ij).eqv..false.) then
          tmp(ij) = .false.
       else
          tmp(ij) = .true.
       endif
    enddo
  END SUBROUTINE xios_bool_to_logical_1d

  SUBROUTINE xios_bool_to_logical_2d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j
    do j=1,ni_(2)
      do i=1,ni_(1)
        if (tmp(i,j).eqv..false.) then
          tmp(i,j) = .false.
        else
          tmp(i,j) = .true.
        endif
      enddo
    enddo
  END SUBROUTINE xios_bool_to_logical_2d

  SUBROUTINE xios_bool_to_logical_3d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k
    do k=1,ni_(3)
      do j=1,ni_(2)
        do i=1,ni_(1)
          if (tmp(i,j,k).eqv..false.) then
            tmp(i,j,k) = .false.
          else
            tmp(i,j,k) = .true.
          endif
        enddo
      enddo
    enddo
  END SUBROUTINE xios_bool_to_logical_3d
  
  SUBROUTINE xios_bool_to_logical_4d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k,l
    do l=1,ni_(4)
      do k=1,ni_(3)
        do j=1,ni_(2)
          do i=1,ni_(1)
            if (tmp(i,j,k,l).eqv..false.) then
              tmp(i,j,k,l) = .false.
            else
              tmp(i,j,k,l) = .true.
            endif
          enddo
        enddo
      enddo
    enddo
  END SUBROUTINE xios_bool_to_logical_4d

  SUBROUTINE xios_bool_to_logical_5d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k,l,m
    do m=1,ni_(5)
      do l=1,ni_(4)
        do k=1,ni_(3)
          do j=1,ni_(2)
            do i=1,ni_(1)
              if (tmp(i,j,k,l,m).eqv..false.) then
                tmp(i,j,k,l,m) = .false.
              else
                tmp(i,j,k,l,m) = .true.
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
  END SUBROUTINE xios_bool_to_logical_5d
  
  SUBROUTINE xios_bool_to_logical_6d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:,:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k,l,m,p
    do p=1,ni_(6)
      do m=1,ni_(5)
        do l=1,ni_(4)
          do k=1,ni_(3)
            do j=1,ni_(2)
              do i=1,ni_(1)
                if (tmp(i,j,k,l,m,p).eqv..false.) then
                  tmp(i,j,k,l,m,p) = .false.
                else
                  tmp(i,j,k,l,m,p) = .true.
                endif
              enddo
            enddo
          enddo
        enddo
      enddo
    enddo
  END SUBROUTINE xios_bool_to_logical_6d
  
  SUBROUTINE xios_bool_to_logical_7d(tmp, ni_)
    IMPLICIT NONE
    LOGICAL (KIND=C_BOOL), INTENT(INOUT) :: tmp(:,:,:,:,:,:,:)
    INTEGER, INTENT(IN) :: ni_(:)
    INTEGER :: i,j,k,l,m,p,q
    do q=1,ni_(7)
      do p=1,ni_(6)
        do m=1,ni_(5)
          do l=1,ni_(4)
            do k=1,ni_(3)
              do j=1,ni_(2)
                do i=1,ni_(1)
                  if (tmp(i,j,k,l,m,p,q).eqv..false.) then
                    tmp(i,j,k,l,m,p,q) = .false.
                  else
                    tmp(i,j,k,l,m,p,q) = .true.
                  endif
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
    enddo
  END SUBROUTINE xios_bool_to_logical_7d
  
END MODULE logical_bool_conversion


