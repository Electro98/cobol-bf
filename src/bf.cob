       IDENTIFICATION DIVISION.
       PROGRAM-ID. bf-cli.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INTERPRETER             CONSTANT "brainfuck-interpreter".
       01 IN-CODE                 PIC IS X(16384)
           VALUE ALL X"00".
       01 SAFE-STOP               PIC IS X
           VALUE IS X"00".
       01 FILENAME                PIC IS X(512).
       01 CODE-PTR                USAGE IS POINTER
           VALUE IS NULL.
       01 CODE-LEN                USAGE IS BINARY-SHORT UNSIGNED.
       01 RESULT-FLAG             PIC IS 9.
    *>    01 FILE-CONTENT            BASED PIC X ANY LENGTH.
       PROCEDURE DIVISION.
        *>    DISPLAY "Input code:"
        *>    ACCEPT IN-CODE
        *>    MOVE FUNCTION LENGTH( FUNCTION TRIM(IN-CODE) ) TO CODE-LEN.
        *>    DISPLAY "Read " CODE-LEN " characters."
        *>    CALL INTERPRETER USING IN-CODE, CODE-LEN.
           DISPLAY "Input filename:"
           INITIALIZE FILENAME.
           ACCEPT FILENAME.
           CALL "READ-BUFFER" USING FILENAME, CODE-PTR, CODE-LEN, RESULT-FLAG.
           DISPLAY "Result flag is " RESULT-FLAG.
           IF RESULT-FLAG IS EQUAL TO ZERO THEN
            *>   SET ADDRESS OF FILE-CONTENT TO CODE-PTR
            *>   DISPLAY FILE-CONTENT
               DISPLAY "ptr " CODE-PTR
           END-IF.
           FREE CODE-PTR.
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
       01 CRF-DEFAULT-FLAG        CONSTANT 128.
    *>    File-handling related
       01 FILE-DATA.
           05 FILE-HANDLE         PIC X(4) USAGE COMP-X.
           05 FILE-SIZE           PIC X(8) USAGE COMP-X.
           05 FILE-READ-SIZE      PIC X(4) USAGE COMP-X.
       LINKAGE SECTION.
       01 FILENAME                PIC IS X(512).
       01 CODE-PTR                USAGE IS POINTER.
       01 CODE-LEN                USAGE IS BINARY-SHORT UNSIGNED.
       01 RESULT-FLAG             PIC IS 9.
    *>    File content buffer that is dynamically allocated
    *>    01 FILE-CONTENT-BUF        PIC X ANY LENGTH.
       PROCEDURE DIVISION USING FILENAME, CODE-PTR, CODE-LEN, RESULT-FLAG.
       1100-INITIALIZE-MEMORY.
           DISPLAY "Initializing the memory.".
           INITIALIZE CODE-LEN
               REPLACING NUMERIC BY ZEROS.
           INITIALIZE RESULT-FLAG
               REPLACING ALPHANUMERIC BY ZERO.
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
           CALL "CBL_OPEN_FILE" USING FILENAME, READ-ONLY-MODE, 0, 0, FILE-HANDLE.
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
           INITIALIZE FILE-SIZE
               REPLACING NUMERIC BY ZEROS.
           CALL "CBL_READ_FILE" USING FILE-HANDLE, FILE-SIZE, 0, CRF-GET-SIZE-FLAG, 0.
           DISPLAY "File size is " FILE-SIZE " bytes".

       3200-ALLOCATE-MEMORY.
           MOVE FILE-SIZE TO CODE-LEN.
           ALLOCATE FILE-SIZE CHARACTERS
               INITIALIZED RETURNING CODE-PTR.
           DISPLAY "Memory allocated " CODE-LEN " bytes at " CODE-PTR " address."
        *>    SET ADDRESS OF FILE-CONTENT-BUF TO CODE-PTR.
           CONTINUE.

       3300-READ-FILE-CONTENT.
           MOVE FILE-SIZE TO FILE-READ-SIZE.
        *>    FIXME: This doesn't work!!!!
           CALL "CBL_READ_FILE" USING FILE-HANDLE, 0, FILE-READ-SIZE, CRF-DEFAULT-FLAG, CODE-PTR.
           DISPLAY "Read " FILE-READ-SIZE " bytes from file".
           IF RETURN-CODE IS EQUAL TO -1 THEN
              DISPLAY "Problems during reading"
           END-IF.

       4100-CLOSE-FILE.
           CALL "CBL_CLOSE_FILE" USING FILE-HANDLE.
           IF RETURN-CODE IS NOT ZERO THEN
              DISPLAY "There were troubles closing the file"
           END-IF.
        *>    CBL_OPEN_FILE
        *>    FIND OUT SIZE, ABORT IF TOO MUCH
        *>    ALLOCATE memory
        *>    READ FILE
        *>    CLOSE FILE
        *>    RETURN
       END PROGRAM READ-BUFFER.
