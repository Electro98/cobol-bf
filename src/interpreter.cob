       IDENTIFICATION DIVISION.
       PROGRAM-ID. brainfuck-interpreter.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MEMORY-SIZE CONSTANT 30000.
       01 MEMORY.
           05 MEMORY-ROW OCCURS MEMORY-SIZE TIMES INDEXED BY MEMORY-PTR.
               10 MEMORY-C        BINARY-CHAR UNSIGNED.
       01 CODE-PTR                BINARY-LONG.
      *>  Temporary values
       01 TEMPORARY-BUFFER.
           05 CUR-CHR             PIC X VALUE ZERO.
           05 OUT-CHR             PIC X VALUE ZERO.
           05 OUT-CHR-R           BINARY-CHAR UNSIGNED.
       01 TMP-SEARCH.
           05 DEPTH               PIC IS 9(2).
       LINKAGE SECTION.
       01 IN-CODE                 PIC IS X ANY LENGTH.
       01 CODE-LEN                BINARY-LONG UNSIGNED.
       PROCEDURE DIVISION USING IN-CODE, CODE-LEN.
       1100-INITIALIZE-INTERPRETER.
           MOVE 1 to CODE-PTR.
           SET MEMORY-PTR TO 1.
           PERFORM 2100-MAIN-LOOP.
           STOP RUN.

       2100-MAIN-LOOP.
           PERFORM 3100-EXECUTE-OPERATION
              UNTIL CODE-PTR IS GREATER THAN CODE-LEN.

       3100-EXECUTE-OPERATION.
           PERFORM 3400-GET-CURRENT-CHAR
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
                 PERFORM 3200-SEARCH-LOOP-END
              END-IF
           WHEN "]"
              IF MEMORY-C(MEMORY-PTR) IS NOT ZERO THEN
                 PERFORM 3300-SEARCH-LOOP-START
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
           END-EVALUATE.
           ADD 1 TO CODE-PTR.

       3200-SEARCH-LOOP-END.
           INITIALIZE DEPTH
              REPLACING NUMERIC DATA BY ZEROS.
           SET CODE-PTR UP BY 1.
           PERFORM
              UNTIL CODE-PTR IS GREATER OR EQUAL TO CODE-LEN
              PERFORM 3400-GET-CURRENT-CHAR
              EVALUATE CUR-CHR
              WHEN "["
                 ADD 1 TO DEPTH
              WHEN "]"
                 IF DEPTH IS ZERO THEN
                    EXIT PARAGRAPH
                 ELSE
                    SUBTRACT 1 FROM DEPTH
                 END-IF
              WHEN OTHER
                 CONTINUE
              END-EVALUATE
              SET CODE-PTR UP BY 1
           END-PERFORM.

       3300-SEARCH-LOOP-START.
           INITIALIZE DEPTH
              REPLACING NUMERIC DATA BY ZEROS.
           SET CODE-PTR DOWN BY 1.
           PERFORM
              UNTIL CODE-PTR IS EQUAL TO ZERO
              PERFORM 3400-GET-CURRENT-CHAR
              EVALUATE CUR-CHR
              WHEN "]"
                 ADD 1 TO DEPTH
              WHEN "["
                 IF DEPTH IS ZERO THEN
                    EXIT PARAGRAPH
                 ELSE
                    SUBTRACT 1 FROM DEPTH
                 END-IF
              WHEN OTHER
                 CONTINUE
              END-EVALUATE
              SET CODE-PTR DOWN BY 1
           END-PERFORM.

       3400-GET-CURRENT-CHAR.
           MOVE IN-CODE(CODE-PTR:1) TO CUR-CHR.

       END PROGRAM brainfuck-interpreter.
