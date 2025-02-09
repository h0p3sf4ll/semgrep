DATA: x TYPE i.
IF x EQ 10.
  WRITE: / 'x equals 10'.
ELSEIF x NE 10.
  WRITE: / 'x is not equal to 10'.
ELSE.
  WRITE: / 'x is unknown'.
ENDIF.