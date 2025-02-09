TRY.
  CALL FUNCTION 'SOME_FUNCTION'.
CATCH cx_sy_error.
  WRITE: / 'Error occurred'.
ENDTRY.