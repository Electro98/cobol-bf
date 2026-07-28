       IDENTIFICATION DIVISION.
       PROGRAM-ID. bf-cli.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INTERPRETER             CONSTANT "brainfuck-interpreter".
       01 IN-CODE                 PIC IS X(4096)
           VALUE ALL X"00".
       01 SAFE-STOP               PIC IS X
           VALUE IS X"00".
       01 CODE-LEN                USAGE BINARY-SHORT UNSIGNED.
       PROCEDURE DIVISION.
           DISPLAY "Input code:"
           ACCEPT IN-CODE
           MOVE FUNCTION LENGTH( FUNCTION TRIM(IN-CODE) ) TO CODE-LEN.
           CALL INTERPRETER USING IN-CODE, CODE-LEN.
       END PROGRAM bf-cli.
