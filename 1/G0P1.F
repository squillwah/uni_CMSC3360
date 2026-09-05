
      PROGRAM average
        IMPLICIT NONE

        real g_sum, g_avg, g_tmp
        integer g_count, in_count
        character(len=8) g_verdict
        character(len=16) student_name

        logical in_valid

c ? Is the carriage return really needed (does T1 negate in completely?)
c '/' adds \n, 
100     format(' ', /, T5, A, /)
101     format(' ', /, T2, "##", 4X, "GRADE")
102     format(' ', T1, "Name    = ", A, /,
     &                  "Grades  = ", I2, /,
     &                  "Average = ", F5.2, //,
     &                  "VERDICT: ", A)

c '$' supresses auto \n (in gfortran) (alternatively use advance='no' in write)
200     format(' ', T1, A, $)
201     format(A16)
202     format(I2)

210     format(' ', T2,  I2,  4X, "", $)
c211     format(F5.2)   Using * instead (handles grade inputs without '.')

        write(*, 100) "==== Average ===="
        
        write(*, 200) "Enter your student's name: "
        read(*, 201) student_name
        
        in_valid = .FALSE. 
        do while (.NOT. in_valid)
            write(*, 200) "Enter the count of grades: "
            read(*, 202) g_count
            in_valid = (g_count .GE. 1 .AND. g_count .LE. 99) 
            if (.NOT. in_valid) write(*, *) 
     &      "! Grade count must be in range 1 -> 99"
        end do

        write(*, 101)
        do in_count = 0, g_count-1
            in_valid = .FALSE. 
            do while (.NOT. in_valid)
                write(*, 210) in_count+1 
                read(*, *) g_tmp
                in_valid = (g_tmp .GE. 0 .AND. g_tmp .LE. 100)
                if (.NOT. in_valid) write(*, *) 
     &          "! Grades values must be in range 0.0 -> 100.0"
            end do
            
            g_sum = g_sum + g_tmp
        end do

        g_avg = g_sum / g_count
        
        select case(nint(g_avg))
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
        end select
        
        write(*, 100) "---- Results ----"
        
        write(*, 102) student_name, g_count, g_avg, g_verdict

        write(*, 100) "...... End ......"

      END PROGRAM average
                
        

