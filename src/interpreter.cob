       IDENTIFICATION DIVISION.
       PROGRAM-ID. brainfuck-interpreter.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MEMORY-SIZE CONSTANT 30000.
       01 MEMORY.
           05 MEMORY-ROW OCCURS MEMORY-SIZE TIMES INDEXED BY MEMORY-PTR.
               10 MEMORY-C        BINARY-CHAR UNSIGNED.
       01 CODE-PTR                BINARY-LONG.
       01 Stack-Max-Size          constant 255.
       01 Stack.
           03 Stack-Pointer       BINARY-CHAR UNSIGNED.
           03 Stack-Cur-Value     BINARY-LONG.
           03 Stack-Memory        OCCURS Stack-Max-Size TIMES.
                   05 filler      BINARY-LONG.
    *>    Temporary values
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
              EVALUATE            TRUE
               *>   Very specific optimization for the common pattern '[-]'
                 WHEN IN-CODE(CODE-PTR:3) IS EQUAL "[-]"
                    MOVE ZERO TO MEMORY-C(MEMORY-PTR)
                    ADD 2 to CODE-PTR
                 WHEN MEMORY-C(MEMORY-PTR) IS ZERO
                    PERFORM 3200-SEARCH-LOOP-END
                 WHEN OTHER
                    PERFORM a200-PUT-VALUE
              END-EVALUATE,
           WHEN "]"
              IF MEMORY-C(MEMORY-PTR) IS NOT ZERO THEN
                 PERFORM a500-READ-VALUE
              ELSE
                 PERFORM a400-DROP-VALUE
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

       a100-INITIALIZE-STACK.
           INITIALIZE Stack
               REPLACING NUMERIC DATA BY ZEROS.

       a200-PUT-VALUE.
           IF Stack-Pointer IS EQUAL TO Stack-Max-Size THEN
              DISPLAY "Stack overflow! Aborting..."
              GOBACK
           END-IF.
           ADD 1 TO Stack-Pointer.
           MOVE CODE-PTR TO Stack-Memory(Stack-Pointer).

       a300-POP-VALUE.
           PERFORM A500-READ-VALUE.
           PERFORM A400-DROP-VALUE.

       a400-DROP-VALUE.
           IF Stack-Pointer IS ZERO THEN
              DISPLAY "Error: Trying to drop nonexistent value from stack. Aborting..."
              GOBACK
           END-IF.
           MOVE 0 TO Stack-Memory(Stack-Pointer).
           SUBTRACT 1 FROM Stack-Pointer.

       a500-READ-VALUE.
           IF Stack-Pointer IS ZERO THEN
              DISPLAY "Error: Trying to read nonexistent value from stack. Aborting..."
              GOBACK
           END-IF.
           MOVE Stack-Memory(Stack-Pointer) TO CODE-PTR.

       END PROGRAM brainfuck-interpreter.
