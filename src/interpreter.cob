       IDENTIFICATION DIVISION.
       PROGRAM-ID. brainfuck-interpreter.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MEMORY-SIZE CONSTANT 255.
       01 MEMORY.
          05 MEMORY-ROW OCCURS MEMORY-SIZE TIMES INDEXED BY MEMORY-PTR.
             10 MEMORY-C          BINARY-CHAR UNSIGNED.
       01 CODE-PTR                BINARY-SHORT.
       01 CUR-CHR                 PIC X VALUE ZERO.
       01 OUT-CHR                 PIC X VALUE ZERO.
       01 OUT-CHR-R               BINARY-CHAR UNSIGNED.
       LINKAGE SECTION.
       01 IN-CODE                 PIC IS X ANY LENGTH.
       01 CODE-LEN                BINARY-SHORT UNSIGNED.
       PROCEDURE DIVISION USING IN-CODE, CODE-LEN.
           MOVE 1 to CODE-PTR.
           SET MEMORY-PTR TO 1.
           PERFORM UNTIL CODE-PTR is GREATER THAN CODE-LEN
              MOVE IN-CODE(CODE-PTR:1) TO CUR-CHR
              EVALUATE CUR-CHR
              WHEN "+"
                 ADD 1 TO MEMORY-C(MEMORY-PTR)
              WHEN "-"
                 SUBTRACT 1 FROM MEMORY-C(MEMORY-PTR)
              WHEN "<"
                 SET MEMORY-PTR DOWN BY 1
              WHEN ">"
                 SET MEMORY-PTR UP BY 1
              WHEN "["
                 IF MEMORY-C(MEMORY-PTR) IS ZERO THEN
                    CALL "SEARCH-CLOSE" USING IN-CODE CODE-LEN CODE-PTR
                 END-IF
              WHEN "]"
                 IF MEMORY-C(MEMORY-PTR) IS NOT ZERO THEN
                    CALL "SEARCH-START" USING IN-CODE CODE-LEN CODE-PTR
                 END-IF
              WHEN "."
                 MOVE MEMORY-C(MEMORY-PTR) TO OUT-CHR-R
                 ADD 1 TO OUT-CHR-R
                 MOVE FUNCTION CHAR(OUT-CHR-R) TO OUT-CHR
                 DISPLAY OUT-CHR WITH NO ADVANCING
               *>   DISPLAY MEMORY-C(MEMORY-PTR)
              WHEN ","
                 DISPLAY "char in"
              WHEN OTHER
                 CONTINUE
              END-EVALUATE
              ADD 1 TO CODE-PTR
           END-PERFORM.
       END PROGRAM brainfuck-interpreter.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-CLOSE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DEPTH                   PIC IS 9(2)
           VALUE ZERO.
       LINKAGE SECTION.
       01 IN-CODE                 PIC IS X ANY LENGTH.
       01 CODE-LEN                BINARY-SHORT UNSIGNED.
       01 CODE-PTR                BINARY-SHORT.
       PROCEDURE DIVISION USING IN-CODE, CODE-LEN, CODE-PTR.
           SET CODE-PTR UP BY 1.
           PERFORM UNTIL CODE-PTR is GREATER OR EQUAL TO CODE-LEN
              EVALUATE IN-CODE(CODE-PTR:1)
              WHEN "["
                 ADD 1 TO DEPTH
              WHEN "]"
                 IF DEPTH IS ZERO THEN
                    GOBACK
                 ELSE
                    SUBTRACT 1 FROM DEPTH
                 END-IF
              WHEN OTHER
                 CONTINUE
              END-EVALUATE
              SET CODE-PTR UP BY 1
           END-PERFORM.
       END PROGRAM SEARCH-CLOSE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-START.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DEPTH                   PIC IS 9(2)
           VALUE ZERO.
       LINKAGE SECTION.
       01 IN-CODE                 PIC IS X ANY LENGTH.
       01 CODE-LEN                BINARY-SHORT UNSIGNED.
       01 CODE-PTR                BINARY-SHORT.
       PROCEDURE DIVISION USING IN-CODE, CODE-LEN, CODE-PTR.
           SET CODE-PTR DOWN BY 1.
           PERFORM UNTIL CODE-PTR is EQUAL TO ZERO
              EVALUATE IN-CODE(CODE-PTR:1)
              WHEN "]"
                 ADD 1 TO DEPTH
              WHEN "["
                 IF DEPTH IS ZERO THEN
                    GOBACK
                 ELSE
                    SUBTRACT 1 FROM DEPTH
                 END-IF
              WHEN OTHER
                 CONTINUE
              END-EVALUATE
              SET CODE-PTR DOWN BY 1
           END-PERFORM.
       END PROGRAM SEARCH-START.
