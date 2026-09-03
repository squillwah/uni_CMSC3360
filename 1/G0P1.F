
*23456
      PROGRAM average
        IMPLICIT NONE

*   Should we use the ::? 
        real grade_sum, grade_avg, grade_tmp
        integer grade_count, input_count
        character student_name*16

10      format(' ', A)
11      format(' ', A, F5.2)
12      format(' ', "[", I3, " /", I3, " ] ", A)

20      format(F5.2)
21      format(I4)
22      format(A)
        
14      format(' ', 'Student Name', T16, "Grades", T24, "Average", /,
     &         T3,           A11,  T17,      I4,  T25,    F5.2 )  

23      format(' ', T1, "Name    = ", A, /,
     &                  "Grades  = ", I3, /,
     &                  "Average = ", F5.2)

        write(*, 10) "Enter the student's name: "
        read(*, 22) student_name
        write(*, 10) "Enter the count of grades: "
        read(*, 21) grade_count
        do while (grade_count .LT. 1 .OR. grade_count .GT. 999)
            write(*, 10) "Grade count must be in range 1 -> 999: "
            read(*, 21) grade_count
        end do
        
        input_count = 0
        do while (input_count < grade_count) 
            write(*, 12) input_count+1, grade_count, "Enter Grade: " 
            read(*, *) grade_tmp
            if (grade_tmp .LT. 0 .OR. grade_tmp .GT. 100) then
                write(*, 10) "Grades must be in range 0.0 -> 100.0"
            else
                grade_sum = grade_sum + grade_tmp
                input_count = input_count + 1
            end if
        end do

        grade_avg = grade_sum / grade_count

        write(*, 11) student_name, grade_avg
        write(*, 23) student_name, grade_count, grade_avg
        write(*, 14) student_name, grade_count, grade_avg

      END PROGRAM average






        
