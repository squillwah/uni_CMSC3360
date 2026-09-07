C234567
      PROGRAM AVERAGE
      IMPLICIT NONE

C Variable Declarations
      CHARACTER(len=20) :: name
      INTEGER :: gradeAmount
      INTEGER :: iterator
      REAL :: gradeTotal
      REAL :: grade
      REAL :: gradeAverage

C Initial variable assignements
      gradeAmount = 0
      gradeTotal = 0
      grade = 0
      gradeAverage = 0

      PRINT *, "Enter name: "
      READ(*,*) name
C. Used this loop to avoid division by zero
      DO WHILE(gradeAmount .EQ. 0)
          PRINT *, "Enter number of grades: "
          READ(*,*) gradeAmount
      END DO

      DO iterator = 1,gradeAmount
        PRINT *, "Enter next grade: "
        READ(*,*) grade
        gradeTotal = gradeTotal + grade

      END DO

      gradeAverage = gradeTotal / gradeAmount
      PRINT *, "Name: " , name
      PRINT 100, "Number of grades: " , gradeAmount
      PRINT 200, "Grade Total: " , gradeTotal
      PRINT 200, "Grade Average: " , gradeAverage

  100 FORMAT(A20,I2)
  200 FORMAT(A20,F5.2)

      STOP
      END PROGRAM AVERAGE
