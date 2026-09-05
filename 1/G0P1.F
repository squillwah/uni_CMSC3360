
      PROGRAM average
        IMPLICIT NONE

c       integer, parameter :: NAME_WIDTH = 16
        INTEGER NAME_WIDTH 
        PARAMETER (NAME_WIDTH = 16)

        real grade_sum, grade_avg, grade_tmp
        integer grade_count, input_count
        character(len=NAME_WIDTH) student_name
        

c       write(*, 

c ? Is the carriage return really needed (does T1 negate in completely?)
c '/' adds \n, 
100     format(' ', /, T5, A, /)
101     format(' ', /, T2, "##", 4X, "GRADE")
102     format(' ', T1, "Name    = ", A, /,
     &                  "Grades  = ", I2, /,
     &                  "Average = ", F5.2)

c '$' supresses auto \n (in gfortran) (alternatively use advance='no' in write)
200     format(' ', T1, A, $)
201     format(A16)
202     format(I2)

210     format(' ', T2,  I2,  4X, "", $)
c211     format(F5.2)   Using * instead (handles grade inputs without '.')

        write(*, 100) "==== Average ===="
        
        write(*, 200) "Enter your student's name: "
        read(*, 201) student_name
        
        write(*, 200) "Enter the count of grades: "
        read(*, 202) grade_count
        
        do while (grade_count .LT. 1 .OR. grade_count .GT. 99) 
            write(*, *) "! Grade count must be in range 1 to 99"
            write(*, 200) "Try again: "
            read(*, 202) grade_count
        end do

        write(*, 101)
        input_count = 0
        do while (input_count < grade_count) 
            write(*, 210) input_count + 1 
            read(*, *) grade_tmp
            
            do while (grade_tmp .LT. 0 .OR. grade_tmp .GT. 100)
                write(*, *) "! Grades must be in range 0.0 -> 100.0"
                write(*, 210) input_count + 1 
                read(*, *) grade_tmp
            end do
            
            grade_sum = grade_sum + grade_tmp
            input_count = input_count + 1
        end do

        grade_avg = grade_sum / grade_count
        
        write(*, 100) "---- Results ----"
        
        write(*, 102) student_name, grade_count, grade_avg

        write(*, 100) "...... End ......"

      END PROGRAM average
                
        

