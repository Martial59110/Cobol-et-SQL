       IDENTIFICATION DIVISION.
       PROGRAM-ID. belg.


       DATA DIVISION.
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
           05 DK-SPOKEN         PIC X(50).
           05 DK-COUNTRY        PIC X(50).
           05 DK-COUNTRY-CODE   PIC X(50).
           05 DK-INFO-PHONE     PIC X(50).  
 
       01  DK-AGE-TABLE.
           05  DK-AGE-ENTRY OCCURS 100 TIMES.
       10  AGE          PIC 99.
       10  COUNTER PIC 99.
         

       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.
           
       PROCEDURE DIVISION.
       EXEC SQL 
           CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
       END-EXEC. 
            IF  SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.
 

      * L'âge maximum

       EXEC SQL 
           SELECT MAX(age) INTO :DK-AGE FROM databank
       END-EXEC. 

           DISPLAY DK-AGE.

      * L'âge minimum

       EXEC SQL 
           SELECT MIN(age) INTO :DK-AGE FROM databank
       END-EXEC.

           DISPLAY DK-AGE. 
      

      * Le nombre d’individus par âge

       
       EXEC SQL
           DECLARE CRAGE CURSOR FOR
           SELECT age , COUNT(*) AS NombreDe
           FROM databank GROUP BY 
           age ORDER BY NombreDe DESC
       END-EXEC.

       EXEC SQL OPEN CRAGE END-EXEC.
       
           PERFORM FETCH-CRAGE
           UNTIL SQLCODE NOT = 0.
      
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX = 32
           DISPLAY "Age: ", AGE(WS-IDX), " Count: ", COUNTER(WS-IDX)
           END-PERFORM.
       EXEC SQL CLOSE CRAGE END-EXEC.
       
           PERFORM UPDATE-TABLE.
     

           STOP RUN. 


       FETCH-CRAGE.
       EXEC SQL
       
       FETCH CRAGE INTO :DK-AGE-ENTRY

       END-EXEC.
           
       UPDATE-TABLE.  
       
      * Met à jour le country code

       EXEC SQL
       UPDATE databank
       SET country_code = 'BE'
       WHERE age > 35 AND age < 40 AND country_code = 'FR'
       END-EXEC.
    
      * Fais correspondre le pays au country code

       EXEC SQL
       UPDATE databank
       SET country = 'Belgique'
       WHERE country-code = 'BE' 
       END-EXEC.

      * Met en majuscule le pays et la langue parlé

       EXEC SQL
       UPDATE databank
       SET country = UPPER(country),
           spoken = UPPER(spoken)
       END-EXEC.

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