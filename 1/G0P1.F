
*                    12.34
#define GRADE_FORMAT F5.2

*23456
      PROGRAM average
        IMPLICIT NONE

*   Should we use the ::? 
        real :: grade_sum, grade_avg, grade_tmp
        integer :: input_limit, input_count
        logical :: input_confirm
        character :: student_name

*   integer, parameter :: PRECISE = 4

10      format(' ', A)
11      format(' ', A, F5.2)
12      format(' ', "[", I4, "/", I4, "] ", A)

20      format(F5.2)
21      format(I4)

        write(*, 10) "Enter the number of grades: " 
        read(*, 21) input_limit
        
        if (input_limit .GE. 1) then
            input_count = 0
            
            do while (input_count < input_limit) 
                write(*, 12) input_count+1, input_limit, "Enter Grade: " 
                read(*, 20) grade_tmp

c               do while (grade_tmp .GE. 100 .OR. grade_tmp .LT. 0)
c                   write(*, 
               
                grade_sum = grade_sum + grade_tmp
                input_count = input_count + 1
            end do
        else
            write(*, 10) "Fine then, leave!"
        end if


c       write(*, 11) "Test ", 12.3456
        write(*, 11) "Test ", grade_sum

      END PROGRAM average






        
