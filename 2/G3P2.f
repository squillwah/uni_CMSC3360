
      PROGRAM Average2
      IMPLICIT NONE

      INTEGER, PARAMETER :: GRADE_LIMIT = 10, NAME_LIMIT = 32
      
      character(len=NAME_LIMIT) :: student_name
      character(len=8) :: grade_verdict
      
      real, dimension(max_grades) :: grades
      real :: grade_tmp, grade_avg
        
      integer i, ios
      logical escape

      student_name = "Standard Stanley"
      grade_verdict = ''
      grades = 0
      grade_avg = 0

* Output / headers
100   FORMAT (' ', /, T5, A, /)
101   FORMAT (' ', /, T2, "##", 4X, "GRADE")
102   FORMAT (' ', T1, "Name    = ", A, /,
     &                 "Grades  = ", I2, /,
     &                 "Sum     = ", F7.2, /,
     &                 "Average = ", F5.2, //,
     &                 "VERDICT: ", A)

* Name prompt + input format
200   FORMAT (' ', T1, A)
201   FORMAT (A)

* Grade prompt
210   FORMAT (' ', T2,  I2,  4X, "")

      i = 1
      DO WHILE ( i .LE. GRADE_LIMIT .AND. .NOT. escape)
          
