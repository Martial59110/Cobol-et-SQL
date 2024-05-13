       IDENTIFICATION DIVISION.
       PROGRAM-ID. belg3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "RAPPORT.txt"
           ORGANIZATION IS LINE SEQUENTIAL 
           ACCESS MODE IS SEQUENTIAL.
           

       DATA DIVISION.
       FILE SECTION.

       FD  OUTPUT-FILE.
       01  ALL-DATA.
           05  DATA-FILE PIC X(300).
          

       WORKING-STORAGE SECTION.
       
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DBNAME PIC X(20) VALUE "dbage".
       01  USERNAME  PIC X(20) VALUE "cobol".
       01  PASSWD PIC X(10) VALUE SPACE.

       01  WS-IDX PIC 99.
       

       01 PHRASE.
           05 PH-COUNTRY-CODE   PIC X(50).
           05 PH-PHRASE         PIC X(50).

       01  DATABANK.
           05 DK-FIRST-NAME     PIC X(50).
           05 DK-LAST-NAME      PIC X(50).
           05 DK-EMAIL          PIC X(50).
           05 DK-GENDER         PIC X(50).
           05 DK-AGE            PIC 9(10).
           05 DK-PROPORTION     PIC 999V99.   
           05 DK-SPOKEN         PIC X(50).
           05 DK-COUNTRY        PIC X(50).
           05 DK-COUNTRY-CODE   PIC X(50).
           05 DK-INFO-PHONE     PIC X(50). 
           05 DK-COUNT          PIC 9(10). 

       01  DONNEES.
           05 AGE-MINIMUM PIC ZZ9.
           05 AGE-MAXIMUM PIC ZZ9.
           05 AGE-MEDIAN PIC ZZ9.
           05 AGE-MEDIAN PIC ZZ9.
           05 COUNTRY    PIC X(20).
           05 GENDER   PIC X(20).
           05 COUNTER  PIC ZZZZZZZ9.
           05 PROPORTION PIC 999.

       01  FULLWRITE.
           05 FILLER PIC X(100) VALUE ALL "*".
       01  HEADLINE.
           05 FILLER PIC X(15) VALUE ALL SPACE.
           05 FILLER PIC X(7) VALUE "Country".
           05 FILLER PIC X(7) VALUE ALL SPACE.

       01  SECONDLINE.
           05 FILLER PIC X(10) VALUE "Population".



       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.
           
       PROCEDURE DIVISION.   
           EXEC SQL 
           CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC. 
           IF  SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF

           
           OPEN OUTPUT OUTPUT-FILE.
           WRITE ALL-DATA FROM FULLWRITE.
           WRITE ALL-DATA FROM HEADLINE.
           WRITE ALL-DATA FROM SPACE.
           WRITE ALL-DATA FROM SECONDLINE.
           EXEC SQL
           SELECT MAX(age) INTO :DK-AGE FROM databank
           END-EXEC.

           MOVE DK-AGE TO AGE-MAXIMUM.

           EXEC SQL
           SELECT MIN(age) INTO :DK-AGE FROM databank
           END-EXEC.

           MOVE DK-AGE TO AGE-MINIMUM.
       
           
      *     EXEC SQL 
      *     SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY age) 
      *     INTO :DK-AGE FROM databank
      *     END-EXEC.

      *     MOVE DK-AGE TO AGE-MEDIAN.
      *     DISPLAY AGE-MEDIAN.

           EXEC SQL
           DECLARE toto CURSOR FOR
           SELECT country, gender, COUNT(*)
           FROM databank
           GROUP BY country, gender
           END-EXEC.

           EXEC SQL OPEN toto END-EXEC.
           DISPLAY SQLCODE.
           PERFORM 1000-FETCH UNTIL SQLCODE NOT = 0.
           EXEC SQL CLOSE toto END-EXEC.
          
       
           STOP RUN.

       1000-FETCH.
           
          
           EXEC SQL
           FETCH toto  INTO :DK-COUNTRY, :DK-GENDER, :DK-COUNT
           END-EXEC.
        
           MOVE DK-COUNTRY TO COUNTRY.
           MOVE DK-GENDER TO GENDER.
           MOVE DK-COUNT TO COUNTER.
           MOVE DK-PROPORTION TO PROPORTION.
           WRITE ALL-DATA FROM COUNTRY
           WRITE ALL-DATA  FROM GENDER
           WRITE ALL-DATA  FROM COUNTER
           WRITE ALL-DATA FROM PROPORTION 
           DISPLAY "PAYS : " COUNTRY "Il y'a" COUNTER SPACE
           GENDER "donc" PROPORTION.

       1001-ERROR-RTN-START.
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE SPACE.
           EVALUATE SQLCODE
              WHEN  +100
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection failed"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
              *> TO RESTART TRANSACTION, DO ROLLBACK.
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       1001-ERROR-RTN-END.
           STOP RUN. 