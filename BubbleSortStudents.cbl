       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 num-students PIC 99 VALUE 5.
       01 i            PIC  9.
       01 j            PIC  9.
       01 k            PIC  9.
       01 m             PIC 9.

       01 STUDENT OCCURS 1 TO 1000 TIMES DEPENDING ON num-students.
          05 STUDENT-NAME      PIC A(25).
          05 STUDENT-AGE       PIC 99.

       01 TEMP-STUDENT.
          05 TEMP-NAME               PIC A(25).
          05 TEMP-AGE                PIC 99.

       PROCEDURE DIVISION.
       MAIN-PARA.
          MOVE "SHARON"    TO STUDENT-NAME (2).
          MOVE 21          TO STUDENT-AGE  (2).

          MOVE "DALE"      TO STUDENT-NAME (3).
          MOVE 21          TO STUDENT-AGE  (3).

          MOVE "CHRISANNE" TO STUDENT-NAME (4).
          MOVE 12          TO STUDENT-AGE  (4).

          MOVE "GREGORY"   TO STUDENT-NAME (1).
          MOVE 87          TO STUDENT-AGE  (1).

          MOVE "ALEX"      TO STUDENT-NAME (5).
          MOVE 12          TO STUDENT-AGE  (5).

          PERFORM PRINT.
          display "start".
          PERFORM SORT-STUDENTS.
          display "done".
          PERFORM PRINT.

       GOBACK.
      *STOP RUN.

      *-----------------------------------------------------------------
       SORT-STUDENTS.
          SET i TO 1
          SET j TO 2
          PERFORM  UNTIL j > num-students
             IF STUDENT-AGE(i) NOT < STUDENT-AGE(j)
                PERFORM MOVE-STUDENTS
             END-IF
             ADD 1                TO j
          END-PERFORM.

       MOVE-STUDENTS.
          IF j > 2
             COMPUTE k = j - i
             display 'bump multiple times = 'k
             PERFORM BUMP-STUDENTS
          ELSE
             MOVE STUDENT(i)   TO TEMP-STUDENT
             MOVE STUDENT(j)   TO STUDENT(i)
             MOVE TEMP-STUDENT TO STUDENT(j)
          END-IF.

       BUMP-STUDENTS.
          MOVE STUDENT(j)   TO TEMP-STUDENT.
          MOVE j TO m.
          PERFORM k TIMES
             MOVE STUDENT(m - 1)   TO STUDENT(m)
             SUBTRACT 1 FROM m
          END-PERFORM.
          MOVE TEMP-STUDENT TO STUDENT(i).

       PRINT.
          DISPLAY '1 = 'STUDENT (1)
          DISPLAY '2 = 'STUDENT (2)
          DISPLAY '3 = 'STUDENT (3)
          DISPLAY '4 = 'STUDENT (4)
          DISPLAY '5 = 'STUDENT (5)
          DISPLAY ' '.


      *         DISPLAY "i age is " STUDENT-AGE(i)
      *         DISPLAY "j age is " STUDENT-AGE(j)

