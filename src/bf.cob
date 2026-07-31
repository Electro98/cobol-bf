       IDENTIFICATION DIVISION.
       PROGRAM-ID. bf-cli.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INTERPRETER             CONSTANT "brainfuck-interpreter".
    *>    For handling files
       01 FILENAME                PIC IS X(512).
       01 TOTAL-FILE-SIZE         PIC X(8) USAGE COMP-X.
       01 RESULT-FLAG             PIC IS 9.
    *>    Main memory
       01 CODE-LEN                BINARY-LONG UNSIGNED.
       01 CODE-BUFFER.
           03 FILLER              PIC IS X(1024) occurs 1024     VALUE ALL x"00".
           03 FILLER              PIC IS X(2)                    VALUE ALL x"00".
    *>    Safeguard just in case
       01 FILLER                  PIC IS X                       VALUE x"00".
       PROCEDURE DIVISION.
           DISPLAY "Input filename:"
           INITIALIZE FILENAME.
           ACCEPT FILENAME.
           CALL "READ-BUFFER" USING
               FILENAME
               CODE-BUFFER
               TOTAL-FILE-SIZE
               RESULT-FLAG
           END-CALL.
           DISPLAY "Result flag is " RESULT-FLAG.
           IF RESULT-FLAG IS EQUAL TO ZERO THEN
               DISPLAY "First 100 bytes " CODE-BUFFER(1:100)
               DISPLAY " ~ OUTPUT ~"
               MOVE TOTAL-FILE-SIZE TO CODE-LEN
               CALL INTERPRETER USING
                   CODE-BUFFER
                   CODE-LEN
               END-CALL
           END-IF.
           STOP RUN.
       END PROGRAM bf-cli.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-BUFFER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 READ-ONLY-MODE          CONSTANT 1.
       01 WRITE-ONLY-MODE         CONSTANT 2.
    *>    CBL_OPEN_FILE return codes
       01 COF-WRONG-ARGUMENT      CONSTANT -1.
       01 COF-SUCCESS             CONSTANT 0.
       01 COF-FILE-DOES-NOT-EXIST CONSTANT 35.
    *>    CBL_READ_FILE related
       01 CRF-GET-SIZE-FLAG       CONSTANT 128.
       01 CRF-DEFAULT-FLAG        CONSTANT 0.
       01 CRF-READ-BLOCK-SIZE     CONSTANT 1024.
    *>    File-handling related
       01 FILE-DATA.
           05 FILE-HANDLE         PIC X(4) USAGE COMP-X.
           05 FILE-READ-OFFSET    PIC X(8) USAGE COMP-X.
    *>    Temporary memory
       01 BUFFER.
           05 BUFFER-LEN          USAGE IS BINARY-LONG UNSIGNED
               VALUE 1024.
           05 BUFFER-PTR          USAGE IS POINTER.
       01 BUFFER-CHUNK            BASED PIC IS X(CRF-READ-BLOCK-SIZE).
       LINKAGE SECTION.
       01 FILENAME                PIC IS X(512).
       01 FILE-BUFFER             PIC X ANY LENGTH.
       01 FILE-READ-BYTES         PIC X(8) USAGE COMP-X.
       01 RESULT-FLAG             PIC IS 9.
       PROCEDURE DIVISION USING FILENAME, FILE-BUFFER, FILE-READ-BYTES, RESULT-FLAG.
       1100-INITIALIZE-MEMORY.
           DISPLAY "Initializing the memory.".
           INITIALIZE FILE-READ-BYTES
               REPLACING ALPHANUMERIC BY ZERO.
           INITIALIZE RESULT-FLAG
               REPLACING ALPHANUMERIC BY ZERO.
           MOVE FUNCTION LENGTH(FILE-BUFFER) TO BUFFER-LEN.
           SET BUFFER-PTR TO ADDRESS OF FILE-BUFFER.
           PERFORM 2100-MAIN-FLOW.
           GOBACK.

       2100-MAIN-FLOW.
           PERFORM 2200-OPEN-FILE.
           PERFORM 3100-READ-FILE-SIZE.
           PERFORM 3200-ALLOCATE-MEMORY.
           PERFORM 3300-READ-FILE-CONTENT.
           PERFORM 4100-CLOSE-FILE.

       2200-OPEN-FILE.
           DISPLAY "Trying to open file".
           INITIALIZE FILE-HANDLE
               REPLACING NUMERIC BY ZEROS.
           CALL "CBL_OPEN_FILE" USING FILENAME, READ-ONLY-MODE, 1, 0, FILE-HANDLE.
           EVALUATE RETURN-CODE
              WHEN COF-WRONG-ARGUMENT
                 DISPLAY "Wrong arguments for calling 'CBL_OPEN_FILE', aborting..."
                 MOVE 1 TO RESULT-FLAG
                 GOBACK
              WHEN COF-FILE-DOES-NOT-EXIST
                 DISPLAY "File doesn't exist, aborting..."
                 MOVE 1 TO RESULT-FLAG
                *>  TODO: Will move logic there
                 PERFORM 2300-FILE-DOES-NOT-EXIST
                 GOBACK
              WHEN COF-SUCCESS
                 DISPLAY "Opened '" FUNCTION TRIM(FILENAME) "'"
           END-EVALUATE.

       2300-FILE-DOES-NOT-EXIST.
           CONTINUE.

       3100-READ-FILE-SIZE.
           INITIALIZE FILE-READ-BYTES
               REPLACING NUMERIC BY ZEROS.
           CALL "CBL_READ_FILE" USING FILE-HANDLE, FILE-READ-BYTES, 0, CRF-GET-SIZE-FLAG, 0.
           DISPLAY "File size is " FILE-READ-BYTES " bytes".
           IF FILE-READ-BYTES IS GREATER THAN BUFFER-LEN THEN
              DISPLAY "File is too big for " BUFFER-LEN " bytes buffer!"
              DISPLAY "Aborting..."
              PERFORM 4100-CLOSE-FILE
              GOBACK
           END-IF.

       3200-ALLOCATE-MEMORY.
           DISPLAY "Allocation is skipped"
           CONTINUE.

       3300-READ-FILE-CONTENT.
           PERFORM UNTIL RETURN-CODE IS EQUAL TO -1 OR 10
                   B-OR FILE-READ-OFFSET IS GREATER THAN FILE-READ-BYTES
               SET ADDRESS OF BUFFER-CHUNK TO BUFFER-PTR
               CALL "CBL_READ_FILE" USING
                   FILE-HANDLE
                   FILE-READ-OFFSET
                   CRF-READ-BLOCK-SIZE
                   CRF-DEFAULT-FLAG
                   BUFFER-CHUNK
               END-CALL
               DISPLAY "Read block. Return code " RETURN-CODE
               ADD CRF-READ-BLOCK-SIZE TO FILE-READ-OFFSET
               SET BUFFER-PTR UP BY CRF-READ-BLOCK-SIZE
               DISPLAY "Next offset " FILE-READ-OFFSET
           END-PERFORM.
           IF RETURN-CODE IS EQUAL TO -1 THEN
              DISPLAY "Problems during reading"
           END-IF.

       4100-CLOSE-FILE.
           CALL "CBL_CLOSE_FILE" USING FILE-HANDLE.
           IF RETURN-CODE IS NOT ZERO THEN
              DISPLAY "There were troubles closing the file"
           END-IF.
       END PROGRAM READ-BUFFER.
