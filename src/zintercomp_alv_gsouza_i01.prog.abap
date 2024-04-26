*&---------------------------------------------------------------------*
*& Include          ZINTERCOMP_ALV_GSOUZA_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'PUSH'.
      CALL SCREEN 1001.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
