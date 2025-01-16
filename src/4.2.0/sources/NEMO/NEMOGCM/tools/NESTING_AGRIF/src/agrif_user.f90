subroutine agrif_user

end subroutine agrif_user

subroutine agrif_initworkspace

end subroutine agrif_initworkspace

subroutine agrif_initvalues

end subroutine agrif_initvalues

SUBROUTINE Agrif_detect( kg, ksizex )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE Agrif_detect ***
      !!----------------------------------------------------------------------
   INTEGER, DIMENSION(2) :: ksizex
   INTEGER, DIMENSION(ksizex(1),ksizex(2)) :: kg 
      !!----------------------------------------------------------------------
   !
   RETURN
   !
END SUBROUTINE Agrif_detect

SUBROUTINE agrif_before_regridding
END SUBROUTINE agrif_before_regridding