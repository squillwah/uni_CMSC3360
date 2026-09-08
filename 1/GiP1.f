****************************************
* Program 1: Average
* Group #i
*
*  Cody Baker 
*  Mikiya Dixon
*  Ravi Dressler
****************************************

      PROGRAM average
      IMPLICIT NONE

      character(len=16) student_name
      character(len=8) g_verdict
      real g_sum, g_avg, g_tmp
      integer g_count
* 'ios' is for READ iostat values. ios > 0 mean bad input. See p662.
* Capturing iostat overrides halting on bad input / type, allows handling.
      integer ios
      integer in_count
      logical in_valid

      student_name="Default Dan"
      g_verdict=''
      g_sum = 0
      g_avg = 0
      g_tmp = 0
      g_count = 0
      ios = 0
      in_count = 0
      in_valid = .FALSE.

C FORMATS

** Output
* ?: Does the carriage control actually do anything in terminal emulators? 
*    Or is it literally just prepending a character? Does T1 negate that?
* Note: The '/' symbol adds implies a newline. 
100   FORMAT (' ', /, T5, A, /)
101   FORMAT (' ', /, T2, "##", 4X, "GRADE")
102   FORMAT (' ', T1, "Name    = ", A, /,
     &                 "Grades  = ", I2, /,
     &                 "Sum     = ", F7.2, /,
     &                 "Average = ", F5.2, //,
     &                 "VERDICT: ", A)
** Prompts
* Note: WRITE statements later sometimes use 'advance="no"' to kill \n.
* A '$' in format is also an option to supress auto \n (in gfortran).
200   FORMAT (' ', T1, A)
210   FORMAT (' ', T2,  I2,  4X, "")

** Input
* Skipped due to the convenience of *.
* In particular: the correct interpretation of whole reals.
* 201   FORMAT (A16) * 202   FORMAT (I2) * 211   FORMAT (F5.2)

C MAIN

      WRITE (*, 100) "==== Average ===="
      
      WRITE (*, 200, advance="no") "Enter your student's name: "
      READ (*, *) student_name

** Input validation pattern. Encore in accumulation loop.
      in_valid = .FALSE. 
      DO WHILE (.NOT. in_valid)
          WRITE (*, 200, advance="no") "Enter the count of grades: "
          READ (*, *, iostat=ios) g_count

** Input only passes when a valid type within the specified bounds is given.
          in_valid = (ios.EQ.0 .AND. g_count.GE.1 .AND. g_count.LE.99)
          IF (.NOT. in_valid) WRITE(*, *) 
     &"! Grade count must be an integer in range 1 -> 99"
      END DO

** Accumulation loop.
      WRITE(*, 101)
      DO in_count = 0, g_count-1
          in_valid = .FALSE. 
          DO WHILE (.NOT. in_valid)
              WRITE (*, 210, advance="no") in_count+1 
              READ (*, *, iostat=ios) g_tmp

              in_valid = (ios.EQ.0 .AND. g_tmp.GE.0 .AND. g_tmp.LE.100)
              IF (.NOT. in_valid) write(*, *) 
     &"! Grades values must a real number be in range 0.0 -> 100.0"
          END DO
          
          g_sum = g_sum + g_tmp
      END DO


** Formula translation.
      g_avg = g_sum / g_count
      
      SELECT CASE (nint(g_avg))
          CASE (:59) 
              g_verdict = "failure"
          CASE (60:69) 
              g_verdict = "D"
          CASE (70:79) 
              g_verdict = "C"
          CASE (80:89) 
              g_verdict = "B"
          CASE (90:) 
              g_verdict = "A"
          CASE DEFAULT
              g_verdict = "magic"
      END SELECT

** Fruit      
      WRITE (*, 100) "---- Results ----"

      WRITE (*, 102) student_name, g_count, g_sum, g_avg, g_verdict

      WRITE (*, 100) "...... End ......"

      STOP
      END PROGRAM average

