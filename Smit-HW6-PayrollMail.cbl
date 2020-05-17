       IDENTIFICATION DIVISION.
       PROGRAM-ID. PayrollMail.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT IN-PAYROLL-FILE
           ASSIGN TO
           '\\client\E$\COBOL\Homework6\Smit-HW6-PayrollIn.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT OUT-PAYROLL-FILE
           ASSIGN TO
           '\\client\E$\COBOL\Homework6\Smit-HW6-PayrollOut.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT IN-MAIL-FILE
           ASSIGN TO
           '\\client\E$\COBOL\Homework6\Smit-HW6-MailIn.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT OUT-MAIL-FILE
           ASSIGN TO
           '\\client\E$\COBOL\Homework6\Smit-HW6-MailOut.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD IN-PAYROLL-FILE.
       01 IN-PAYROLL-REC.
           05 IN-EMPLOYEE-NUM      PIC X(5).
           05 IN-EMPLOYEE-NAME     PIC X(20).
           05 IN-TERRITORY-NUM     PIC X(2).
           05 IN-OFFICE-NUM        PIC X(2).
           05 IN-ANNUAL-SALARY     PIC X(6).
           05 IN-PHONE-NUM         PIC X(10).

       FD OUT-PAYROLL-FILE.
       01 OUT-HEADER-ONE           PIC X(80).
       01 OUT-HEADER-TWO           PIC X(80).
       01 OUT-BLANKS-ONE           PIC X(80).
       01 OUT-EMPLOYEE-REC         PIC X(80).
       01 OUT-BLANKS-TWO           PIC X(80).

       FD IN-MAIL-FILE.
       01 IN-MAIL-REC.
           05 IN-CUSTOMER-NAME     PIC X(20).
           05 IN-STREET-ADDRESS    PIC X(20).
           05 IN-CITY-STATE-ZIP    PIC X(20).

       FD OUT-MAIL-FILE.
       01 OUT-MAIL-HEADER          PIC X(45).
       01 OUT-BLANK-LINE           PIC X(45).
       01 OUT-CUSTOMER-NAME        PIC X(45).
       01 OUT-STREET-ADDRESS       PIC X(45).
       01 OUT-CITY-STATE-ZIP       PIC X(45).

       WORKING-STORAGE SECTION.
       01 ARE-THERE-MORE-RECORDS   PIC X(3)  VALUE 'YES'.
       01 OUT-PAYROLL-HEADER-ONE.
           05 FILLER               PIC X(28) VALUE SPACES.
           05 PAYROLL              PIC X(15) VALUE 'PAYROLL LISTING'.
           05 FILLER2              PIC X(17) VALUE SPACES.
           05 PAGE-NUM             PIC X(8)  VALUE 'PAGE 01'.
           05 FILLER3              PIC X(2)  VALUE SPACES.
           05 DATE-OUT             PIC X(10).
       01 OUT-PAYROLL-HEADER-TWO.
           05 NUMBER-HEADER        PIC X(10) VALUE 'EMP. NO.  '.
           05 EXTRA_SPACE          PIC X(1)  VALUE SPACES.
           05 NAME-HEADER          PIC X(13) VALUE 'EMPLOYEE NAME'.
           05 FILLER4              PIC X(9)  VALUE SPACES.
           05 TERR-HEADER          PIC X(8)  VALUE 'TERR NO.'.
           05 MORE-SPACE           PIC X(2)  VALUE SPACES.
           05 OFFICE-HEADER        PIC X(10) VALUE 'OFFICE NO.'.
           05 FILLER5              PIC X(2)  VALUE SPACES.
           05 SALARY-HEADER        PIC X(13) VALUE 'ANNUAL SALARY'.
           05 FILLER6              PIC X(2)  VALUE SPACES.
           05 PHONE-HEADER         PIC X(11) VALUE 'PHONE NUM. '.
       01 WORKING-EMPLOYEE-REC.
           05 OUT-EMPLOYEE-NUM     PIC X(5).
           05 OUT-SPACES           PIC X(6).
           05 OUT-EMPLOYEE-NAME    PIC X(20).
           05 OUT-SPACES-TWO       PIC X(2).
           05 OUT-TERRITORY-NUM    PIC X(2).
           05 OUT-SPACES-THREE     PIC X(8).
           05 OUT-OFFICE-NUM       PIC X(2).
           05 OUT-SPACES-FOUR      PIC X(10).
           05 OUT-ANNUAL-SALARY    PIC X(6).
           05 OUT-SPACES-FIVE      PIC X(9).
           05 OUT-PHONE-NUM        PIC X(10).
       01 CURRENTDATE.
           05  YEAR                PIC 9(4).
           05  MONTH               PIC 9(2).
           05  TODAY               PIC 9(2).
           05  TIMESTAMP           PIC 9(12).
       01 WORKING-DATE-OUT.
           05 MONTH-OUT            PIC 9(2).
           05 SLASH-ONE-OUT        PIC X.
           05 TODAY-OUT            PIC 9(2).
           05 SLASH-TWO-OUT        PIC X.
           05 YEAR-OUT             PIC 9(4).
       01 WORKING-MAIL-HEADER.
           05 BLANKS1              PIC X(8)  VALUE SPACES.
           05 MAIL-TITLE           PIC X(12) VALUE 'MAILING LIST'.
           05 BLANKS2              PIC X(5)  VALUE SPACES.
           05 PAGE-NUMBER          PIC X(8)  VALUE 'PAGE: 01'.
           05 BLANKS3              PIC X(2)  VALUE SPACES.
           05 DATE-OUT-2           PIC X(10).
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
       OPEN INPUT  IN-PAYROLL-FILE
            INPUT  IN-MAIL-FILE
            OUTPUT OUT-PAYROLL-FILE
            OUTPUT OUT-MAIL-FILE

       PERFORM 200-PROCESS-RTN
       PERFORM 300-PROCESS-RTN
       PERFORM 400-PROCESS-RTN

       PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
           READ IN-PAYROLL-FILE
               AT END
                   MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
               NOT AT END
                   PERFORM 500-PROCESS-RTN
           END-READ
       END-PERFORM

       MOVE 'YES' TO ARE-THERE-MORE-RECORDS

       PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
           READ IN-MAIL-FILE
               AT END
                   MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
               NOT AT END
                   PERFORM 600-PROCESS-RTN
                   PERFORM 700-PROCESS-RTN
                   PERFORM 800-PROCESS-RTN
                   PERFORM 900-PROCESS-RTN
           END-READ
       END-PERFORM

       CLOSE IN-PAYROLL-FILE
             IN-MAIL-FILE
             OUT-PAYROLL-FILE
             OUT-MAIL-FILE

       STOP RUN.
       200-PROCESS-RTN.
           MOVE FUNCTION CURRENT-DATE TO CURRENTDATE
           MOVE MONTH TO MONTH-OUT
           MOVE '\' TO SLASH-ONE-OUT
           MOVE TODAY TO TODAY-OUT
           MOVE '\' TO SLASH-TWO-OUT
           MOVE YEAR TO YEAR-OUT
           MOVE WORKING-DATE-OUT TO DATE-OUT
           MOVE WORKING-DATE-OUT TO DATE-OUT-2
           MOVE OUT-PAYROLL-HEADER-ONE TO OUT-HEADER-ONE
           WRITE OUT-HEADER-ONE.

       300-PROCESS-RTN.
           MOVE OUT-PAYROLL-HEADER-TWO TO OUT-HEADER-TWO
           WRITE OUT-HEADER-TWO.

       400-PROCESS-RTN.
           MOVE  WORKING-MAIL-HEADER TO OUT-MAIL-HEADER
           WRITE OUT-MAIL-HEADER.

       500-PROCESS-RTN.
           MOVE IN-EMPLOYEE-NUM TO OUT-EMPLOYEE-NUM
           MOVE SPACES TO OUT-SPACES
           MOVE IN-EMPLOYEE-NAME TO OUT-EMPLOYEE-NAME
           MOVE SPACES TO OUT-SPACES-TWO
           MOVE IN-TERRITORY-NUM TO OUT-TERRITORY-NUM
           MOVE SPACES TO OUT-SPACES-THREE
           MOVE IN-OFFICE-NUM TO OUT-OFFICE-NUM
           MOVE SPACES TO OUT-SPACES-FOUR
           MOVE IN-ANNUAL-SALARY TO OUT-ANNUAL-SALARY
           MOVE SPACES TO OUT-SPACES-FIVE
           MOVE IN-PHONE-NUM TO OUT-PHONE-NUM
           MOVE WORKING-EMPLOYEE-REC TO OUT-EMPLOYEE-REC
           WRITE OUT-EMPLOYEE-REC.

       600-PROCESS-RTN.
           MOVE SPACES TO OUT-BLANK-LINE
           WRITE OUT-BLANK-LINE.

       700-PROCESS-RTN.
           MOVE IN-CUSTOMER-NAME TO OUT-CUSTOMER-NAME
           WRITE OUT-CUSTOMER-NAME.

       800-PROCESS-RTN.
           MOVE IN-STREET-ADDRESS TO OUT-STREET-ADDRESS
           WRITE OUT-STREET-ADDRESS.

       900-PROCESS-RTN.
           MOVE IN-CITY-STATE-ZIP TO OUT-CITY-STATE-ZIP
           WRITE OUT-CITY-STATE-ZIP.
