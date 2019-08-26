       IDENTIFICATION DIVISION.
       PROGRAM-ID. cortes_01.
       AUTHOR. Roxelle H. Cortes.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           77 X PIC 99 VALUE 1.
           77 Y PIC 99 VALUE 1.
           77 Z PIC 99.
           77 SN PIC X(10).
           77 CHOICE PIC 9.
           01 students.
               02 student-info OCCURS 5 TIMES.
                  03 name PIC X(30).
                  03 sNum PIC X(10).
                  03 course PIC X(20).
                  03 contact-info.
                     04 mobile PIC X(11).
                     04 landline PIC X(8).
                  03 age PIC 99.

       PROCEDURE DIVISION.
           PERFORM MENU UNTIL CHOICE = 6.
           STOP RUN.
           
           MENU.
               DISPLAY "MENU".
               DISPLAY "[1] Add student".
               DISPLAY "[2] Edit student".
               DISPLAY "[3] Delete student".
               DISPLAY "[4] View info of one student".
               DISPLAY "[5] View info of all students".
               DISPLAY "[6] Exit".
              
               DISPLAY "Choice: " WITH NO ADVANCING.
               ACCEPT CHOICE.

               IF CHOICE = 1
                  DISPLAY "Add A Student"
                  COMPUTE Y = 1
                  PERFORM ADDSTUD UNTIL Y = 6
               END-IF.
              
               IF CHOICE = 2
                  DISPLAY "Edit A Student"
                  COMPUTE Y = 1
                  DISPLAY "Student Number: " WITH NO ADVANCING
                  ACCEPT SN
                  PERFORM EDITSTUD UNTIL Y = 6
               END-IF.
              
               IF CHOICE = 3
                  DISPLAY "Delete A Student"
                  COMPUTE Y = 1
                  DISPLAY "Student Number: " WITH NO ADVANCING
                  ACCEPT SN
                  PERFORM DELETESTUD UNTIL Y = 6
               END-IF.
              
               IF CHOICE = 4
                  DISPLAY "View One Student"
                  COMPUTE Y = 1
                  DISPLAY "Student Number: " WITH NO ADVANCING
                  ACCEPT SN
                  PERFORM VIEWONE UNTIL Y = 6
               END-IF.
              
               IF CHOICE = 5
                  DISPLAY "View All Students"
                  COMPUTE Y = 1
                  PERFORM VIEWALL UNTIL Y = 6
               END-IF.
              
               IF CHOICE = 6
                  COMPUTE CHOICE = 6
               END-IF.




               ADDSTUD.
                  IF X = 6
                     DISPLAY "There are already 5 students recorded."
                     COMPUTE Y = 6
                  ELSE
                     IF sNum(Y) = "          "
                        DISPLAY "Fullname: " WITH NO ADVANCING
                        ACCEPT name(X)
                        DISPLAY name(X)
                        DISPLAY "Student Number: " WITH NO ADVANCING
                        ACCEPT sNum(X)
                        DISPLAY sNum(X)
                        DISPLAY "Course: " WITH NO ADVANCING
                        ACCEPT course(X)
                        DISPLAY course(X)
                        DISPLAY "Mobile No.: " WITH NO ADVANCING
                        ACCEPT mobile(X)
                        DISPLAY mobile(X)
                        DISPLAY "Landline: " WITH NO ADVANCING
                        ACCEPT landline(X)
                        DISPLAY landline(X)
                        DISPLAY "Age: " WITH NO ADVANCING
                        ACCEPT age(X)
                        DISPLAY age(X)
                        COMPUTE X = X + 1
                        COMPUTE Y = 6
                    ELSE
                        COMPUTE Y = Y + 1
                    END-IF
                  END-IF.
                 

               EDITSTUD.
                  IF SN = sNum(Y)
                     DISPLAY "[1] Edit Course"
                     DISPLAY "[2] Edit Mobile No."
                     DISPLAY "[3] Edit Landline"
                     DISPLAY "[4] Edit Age"
                     
                     DISPLAY "Choice: " WITH NO ADVANCING
                     ACCEPT CHOICE

                     IF CHOICE = 1
                        DISPLAY "New Course: " WITH NO ADVANCING
                        ACCEPT course(Y)
                     END-IF
                     IF CHOICE = 2
                        DISPLAY "New Mobile No.: " WITH NO ADVANCING
                        ACCEPT mobile(Y)
                     END-IF
                     IF CHOICE = 3
                        DISPLAY "New Landline: " WITH NO ADVANCING
                        ACCEPT landline(Y)
                     END-IF
                     IF CHOICE = 4
                        DISPLAY "New Age: " WITH NO ADVANCING
                        ACCEPT age(Y)
                     END-IF

                     COMPUTE Y = 6
                     COMPUTE CHOICE = 0
                  ELSE
                     COMPUTE Y = Y + 1
                  END-IF.


               DELETESTUD.
                  IF SN = sNum(Y)
                     MOVE "                              " TO name(Y)
                     MOVE "          " TO sNum(Y)
                     MOVE "                    " TO course(Y)
                     MOVE "           " TO mobile(Y)
                     MOVE "        " TO landline(Y)
                     COMPUTE age(Y) = 00
                     COMPUTE X = X - 1
                     COMPUTE Y = 6
                  ELSE
                     COMPUTE Y = Y + 1
                  END-IF.


               VIEWONE.
                  IF SN = sNum(Y)
                     
                     DISPLAY "--------------------------------------"
                     DISPLAY "Fullname: " WITH NO ADVANCING
                     DISPLAY name(Y)
                     DISPLAY "Student Number: " WITH NO ADVANCING
                     DISPLAY sNum(Y)
                     DISPLAY "Course: " WITH NO ADVANCING
                     DISPLAY course(Y)
                     DISPLAY "Mobile No.: " WITH NO ADVANCING
                     DISPLAY mobile(Y)
                     DISPLAY "Landline: " WITH NO ADVANCING
                     DISPLAY landline(Y)
                     DISPLAY "Age: " WITH NO ADVANCING
                     DISPLAY age(Y)

                     COMPUTE Y = 6
                  ELSE
                     COMPUTE Y = Y + 1
                  END-IF.


               VIEWALL.
                  DISPLAY "--------------------------------------".
                  DISPLAY "Fullname: " WITH NO ADVANCING.
                  DISPLAY name(Y).
                  DISPLAY "Student Number: " WITH NO ADVANCING.
                  DISPLAY sNum(Y).
                  DISPLAY "Course: " WITH NO ADVANCING.
                  DISPLAY course(Y).
                  DISPLAY "Mobile No.: " WITH NO ADVANCING.
                  DISPLAY mobile(Y).
                  DISPLAY "Landline: " WITH NO ADVANCING.
                  DISPLAY landline(Y).
                  DISPLAY "Age: " WITH NO ADVANCING.
                  DISPLAY age(Y).

                  COMPUTE Y = Y + 1.


