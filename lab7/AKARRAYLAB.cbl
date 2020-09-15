       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ArrayLab.
       AUTHOR.        Yourname Here.
      ******************************************************************
      *        CS370           ARRAY LAB
      *
      *   IN THIS LAB YOU WILL ARE WORKING WITH TRAIN STATION DATA.
      *   THERE ARE 3 TRAIN STATIONS IN EACH STATE.YOUR INPUT FILE HAS:
      *   TRAIN STATE  XX
      *   STATION CODE XXXXXX
      *   STATION MANAGER XXXXXXXXXXXXXXX
      *   STATION CITY XXXXXXXXXX
      *   STATION REPAIR CODE XXXXXX
      *     
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT TRAIN-FILE
                ASSIGN TO 'TRAIN.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT REPORT-FILE
                ASSIGN TO 'REPORTXXX.TXT'.

       DATA DIVISION.
       FILE SECTION.

       FD TRAIN-FILE.

       01 TRAIN-ITEM.
          05 TI-TRAIN-STATE        PIC X(2).

      *  THERE ARE 3 TRAIN STATIONS IN EACH STATE
      * CODE THE ARRAY HERE EACH TRAIN STATION 

          05 TI-STATION-ARRAY OCCURS 3 TIMES.
             10 TI-STATION-CODE PIC X(6).
             10 TI-STATION-MANAGER PIC X(15).
             10 TI-STATION-CITY PIC X(10).
             10 TI-STATION-REPAIR PIC X(6).

       FD REPORT-FILE.

       01 REPORT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.

       01 END-OF-FILE-FLAG  PIC X  VALUE SPACE.
          88 MORE-RECORDS            VALUE 'Y'.
          88 NO-MORE-RECORDS         VALUE 'N'.
       01 PROPER-SPACING PIC 9 VALUE 1.

      *  CODE TRAIN-SUB HERE
       01 TRAIN-SUB PIC 9.

       01 HEADER-LINE-1.
          05        PIC X(10) VALUE 'State'.
          05        PIC X(15)  VALUE '   City   '.
          05        PIC X(11)  VALUE 'Repair Code'.

      *  THERE IS NO ARRAY IN THE DETAIL YOU WILL PRINT
      *  A DETAIL LINE FOR EACH ARRAY ELEMENT

       01 DETAIL-LINE.
          05                      PIC X(2) VALUE SPACES.
          05 DL-STATION-STATE     PIC X(2).
          05                      PIC X(8)  VALUE SPACES.
          05 DL-CITY              PIC X(10).
          05                      PIC X(5) VALUE SPACES.
          05 DL-REPAIR-CODE       PIC X(6).




       PROCEDURE DIVISION.

       100-MAIN.

           PERFORM 200-HOUSEKEEPING
           PERFORM 300-READ-ROUTINE
           PERFORM 600-EOJ-ROUTINE
           STOP RUN

          .

       200-HOUSEKEEPING.

           OPEN INPUT TRAIN-FILE
                OUTPUT REPORT-FILE
           PERFORM 700-PRINT-THE-HEADERS

          .

       300-READ-ROUTINE.

           PERFORM UNTIL NO-MORE-RECORDS
           READ TRAIN-FILE
               AT END
                   MOVE 'N' TO END-OF-FILE-FLAG
               NOT AT END
                   PERFORM 400-PROCESS-ROUTINE
            END-READ
            END-PERFORM
          .

       400-PROCESS-ROUTINE.

           MOVE TI-TRAIN-STATE TO DL-STATION-STATE
      *  USE A PERFORM VARYING TO MOVE THE CITY AND REPAIR
      *  CODE TO THE DETAIL LINE AND WRITE IT
  
          PERFORM VARYING TRAIN-SUB FROM 1 BY 1 UNTIL
                TRAIN-SUB > 3
                MOVE TI-STATION-CITY(TRAIN-SUB) TO DL-CITY
                MOVE TI-STATION-REPAIR(TRAIN-SUB) TO DL-REPAIR-CODE
  
                WRITE REPORT-RECORD FROM DETAIL-LINE AFTER
                 ADVANCING PROPER-SPACING
           END-PERFORM

          .
  
  
       600-EOJ-ROUTINE.
           CLOSE TRAIN-FILE
                 REPORT-FILE
          .
       700-PRINT-THE-HEADERS.
           WRITE REPORT-RECORD FROM HEADER-LINE-1
           AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING

          .


