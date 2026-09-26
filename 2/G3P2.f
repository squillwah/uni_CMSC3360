****************************************
* Program 2: Average Array
* Group 3
*
*  Cody Baker 
*  Mikiya Dixon
*  Ravi Dressler
****************************************
      PROGRAM AVERAGEARRAY
      IMPLICIT NONE

* Variable Declarations and Initializations       
      INTEGER, PARAMETER :: MAX_GRADES = 10
      CHARACTER(len=20) :: Name
      REAL :: grades(MAX_GRADES) , SUM , AVERAGE , grade
      INTEGER :: gradeCount ,iterator

      grades = 0.0
      SUM = 0.0
      AVERAGE = 0.0
      gradeCount = 0
      grade = 0.0

* Prompts user for name and grades
      WRITE(*,'(A)',ADVANCE='NO') "Name: "
      READ(*,'(A)') Name

      WRITE(*,'(A)')"Enter up to 10 grades in range of 0 - 100"
      WRITE(*,'(A)')"Enter any other value to stop"

* Continue reading grades until maximum number of grades is reached
* or user enters value outside of range
      DO WHILE (grade .GE. 0 
     & .AND. grade .LE. 100 
     & .AND. gradeCount .LT. MAX_GRADES)

          
          
          READ * , grade

* Only adds grade if it is within range
          IF(grade .GE. 0 .AND. grade .LE. 100) THEN
            gradeCount = gradeCount + 1
            grades(gradeCount) = grade
          END IF

      END DO

* Calculates SUM and AVERAGE
      DO iterator = 1, gradeCount
        SUM = SUM + grades(iterator)
      END DO

      IF(gradeCount .GT. 0) THEN
          AVERAGE = SUM / gradeCount
      END IF

* Begin output
      WRITE(*,100) Name, SUM

      WRITE(*,'(A)')"GRADES: "
      DO iterator = 1,gradeCount
        WRITE(*,200) grades(iterator)
      END DO

      WRITE(*,300) AVERAGE

* Output formatting
  100 FORMAT(/,"Name:",4X, A,/,"SUM:",4X, F7.2)

  200 FORMAT(8X,F7.2)

  300 FORMAT("AVERAGE:",X,F7.2)

      STOP
      END PROGRAM AVERAGEARRAY


