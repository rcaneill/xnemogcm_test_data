! ==============================================================================

PROGRAM scripgrid

  USE scripgrid_mod

  CHARACTER(char_len) :: infile

  if (COMMAND_ARGUMENT_COUNT() == 1) then
    CALL GET_COMMAND_ARGUMENT(1, infile)
  else
  write(6,*) 'enter name of namelist file'
  read(5,*) infile
  endif

  CALL convert( infile )

END PROGRAM scripgrid
