
      PROGRAM average
        IMPLICIT NONE

        real g_sum, g_avg, g_tmp
        integer g_count
        character(len=8) g_verdict
        character(len=16) student_name

        logical in_valid
        integer in_count

c Holds read error status from read(). This overrides crashing with incompatible io types. 
c Values > 0 mean bad io. See p662.
        integer ios

c ? Is the carriage return really needed (does T1 negate in completely?)
c '/' adds \n, 
100     FORMAT (' ', /, T5, A, /)
101     FORMAT (' ', /, T2, "##", 4X, "GRADE")
102     FORMAT (' ', T1, "Name    = ", A, /,
     &                   "Grades  = ", I2, /,
     &                   "Sum     = ", F7.2, /,
     &                   "Average = ", F5.2, //,
     &                   "VERDICT: ", A)

c '$' in format is also an option to supress auto \n (in gfortran).
200     FORMAT (' ', T1, A)
c201     FORMAT (A16)
c202     FORMAT (I2)

210     FORMAT (' ', T2,  I2,  4X, "")
c211     format(F5.2)   Using * instead (handles grade inputs without '.')

        WRITE (*, 100) "==== Average ===="
        
        WRITE (*, 200, advance="no") "Enter your student's name: "
        READ (*, *) student_name

        in_valid = .FALSE. 
        DO WHILE (.NOT. in_valid)
            WRITE (*, 200, advance="no") "Enter the count of grades: "
            READ (*, *, iostat=ios) g_count

            in_valid = 
     & (g_count .GE. 1 .AND. g_count .LE. 99 .AND. ios .EQ. 0)
            IF (.NOT. in_valid) WRITE(*, *) 
     & "! Grade count must be an integer in range 1 -> 99"
        END DO

        WRITE(*, 101)
        DO in_count = 0, g_count-1
            in_valid = .FALSE. 
            DO WHILE (.NOT. in_valid)
                WRITE (*, 210, advance="no") in_count+1 
                READ (*, *, iostat=ios) g_tmp

                in_valid = 
     & (g_tmp .GE. 0 .AND. g_tmp .LE. 100 .AND. ios .EQ. 0)
                IF (.NOT. in_valid) write(*, *) 
     & "! Grades values must a real number be in range 0.0 -> 100.0"
            END DO
            
            g_sum = g_sum + g_tmp
        END DO

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
        
        WRITE (*, 100) "---- Results ----"

        WRITE (*, 102) student_name, g_count, g_sum, g_avg, g_verdict

        WRITE (*, 100) "...... End ......"

      STOP
      END PROGRAM average
                
        

