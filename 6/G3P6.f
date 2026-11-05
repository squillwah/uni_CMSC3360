
****************************************
* Program 6: Complex Equation Solver
* Group 3
*
*  Cody Baker 
*  Mikiya Dixon
*  Ravi Dressler
****************************************

      PROGRAM ASSIGNMENT6
        IMPLICIT NONE

* If MAXEQ is changed, 10 needs to be changed manually in formats.
        INTEGER, PARAMETER :: MAXEQ = 10
        CHARACTER, PARAMETER :: abc(MAXEQ) = ['A','B','C','D','E',
     &                                        'F','G','H','I','J']
        
        TYPE :: System
          INTEGER :: size
          COMPLEX, DIMENSION(:,:), POINTER :: equations
        END TYPE

        TYPE (System) :: sys
        LOGICAL :: error, solve
        CHARACTER :: input
        INTEGER :: i

        sys%size = 0
        NULLIFY (sys%equations)

        error = .FALSE.
        solve = .TRUE.

        DO WHILE (solve)
          CALL create_system (sys)
          CALL read_system (sys)
          CALL write_system (sys)

          WRITE (*, '(A)') "Does that look right (y/*)?"
          READ (*, *) input
          DO WHILE (input .NE. 'Y' .AND. input .NE. 'y')
            WRITE (*, '(A)') "Reenter: "
            CALL read_system (sys)
            CALL write_system (sys)
            WRITE (*, '(A)') "Does that look right (y/*) (A to abort)?"
            READ (*, *) input 
            IF (input .EQ. 'A') solve = .FALSE.
          END DO

          IF (solve) THEN 
            solve = .FALSE.
            CALL solve_system (sys, error)
            IF (error) WRITE (*, *) "! An error occured, by the way."
         
            WRITE (*, *) "The reduced system: "   
            CALL write_system (sys)
            WRITE (*, *) "The solution vector: "
            DO i = 1, sys%size
              WRITE (*, '(A, ": ")', advance='no') abc(i)
              CALL write_imaginary(sys%equations(i, sys%size+1))
              WRITE (*, *) ''
            END DO
          END IF

          CALL destroy_system (sys)

          WRITE (*, '(A)') "Would you like to enter another (y/*)?"
          READ (*, *) input
          IF (input .EQ. 'Y' .OR. input .EQ. 'y') solve = .TRUE.
        END DO

      CONTAINS

      SUBROUTINE create_system (sys)
        IMPLICIT NONE
        INTEGER :: ios
        TYPE (System), INTENT(OUT) :: sys
        
        WRITE (*, '(A)') "How many equations?"
        READ (*, *, iostat=ios) sys%size
        DO WHILE (ios .NE. 0 .OR. 
     &            sys%size .LT. 1 .OR. sys%size .GT. MAXEQ)
          WRITE (*, '(A)') "Try that again (1 -> 10):"
          READ (*, *, iostat=ios) sys%size
        END DO

* Plus one column for = constants.
        ALLOCATE (sys%equations(sys%size, sys%size+1))
        sys%equations = (0.0,0.0)
      END SUBROUTINE

      SUBROUTINE destroy_system (sys)
        IMPLICIT NONE
        TYPE (System), INTENT(INOUT) :: sys

        sys%size = 0
        DEALLOCATE (sys%equations)
        NULLIFY (sys%equations)
      END SUBROUTINE


* Make sure to dealloc system before this is called. 
      SUBROUTINE read_system (sys)
        IMPLICIT NONE 
        INTEGER :: ios, i, j
        REAL :: c(2)
        TYPE (System), INTENT(INOUT) :: sys

* Nice diagram.
        WRITE (*, '(2X, 12(X, A))') abc(1:sys%size), '=', 'c'
        DO i = 1, sys%size
          WRITE (*, '(I2, 11(X, A))') i,[('.', j = 1, sys%size)],' ','.'
        END DO

        WRITE (*, '(A)') "Enter Coefficients Below: "
        DO i = 1, sys%size
          DO j = 1, sys%size
            WRITE (*, '(I2, A, ": ")', advance='no') i, abc(j)
            READ (*, *, iostat=ios) c
            DO WHILE (ios .NE. 0)
              WRITE (*, *) "! Try that one again."
              READ (*, *, iostat=ios) c
            END DO
            sys%equations(i, j) = cmplx(c(1), c(2))
          END DO
        END DO

        WRITE (*, '(A)') "Enter Constants Below: "
        DO i = 1, sys%size
          WRITE (*, '(I2, "c: ")', advance='no') i
          READ (*, *, iostat=ios) c
          DO WHILE (ios .NE. 0)
            WRITE (*, *) "! Try that one again."
            READ (*, *, iostat=ios) c
          END DO
          sys%equations(i, sys%size+1) = cmplx(c(1), c(2))
        END DO

      END SUBROUTINE

      SUBROUTINE write_system (sys)
        IMPLICIT NONE 
        INTEGER :: ios, i, j
        TYPE (System), INTENT(IN) :: sys

        WRITE (*, '(I2, ": ")', advance='no') 
        DO i = 1, sys%size
          DO j = 1, sys%size-1
            CALL write_imaginary(sys%equations(i,j))
            WRITE (*, '("x", A, " + ")', advance='no') abc(j)
          END DO
          CALL write_imaginary(sys%equations(i,sys%size))
          WRITE (*, '("x", A, " = ")', advance='no') abc(j)
          CALL write_imaginary(sys%equations(i,sys%size+1))
          WRITE (*, *) 'c'
        END DO
      END SUBROUTINE

      SUBROUTINE write_imaginary (num)
        IMPLICIT NONE
        CHARACTER(len=12) :: numtrim(2)
        COMPLEX, INTENT(IN) :: num

        WRITE (numtrim(1), '(F8.3)') real(num) 
        WRITE (numtrim(2), '(F8.3)') aimag(num)
        WRITE (*, "('(',A,' +i',A,')')", advance='no')
     &  trim(adjustl(numtrim(1))), trim(adjustl(numtrim(2)))
      END SUBROUTINE


* From book.
      SUBROUTINE solve_system (sys, error)
        IMPLICIT NONE
* 'Comparison threshold.'
        REAL, PARAMETER :: EPSILON = 1.0E-6
        TYPE (System), INTENT(INOUT) :: sys
        LOGICAL, INTENT(OUT) :: error
        INTEGER :: r, ri, peak
        COMPLEX :: temprow(sys%size+1)

* Rows, top to bottom.
        DO r = 1, sys%size
* Find the highest magnitude coeff for current pivot (pivot is r, diagonal).
          peak = r
          DO ri = r+1, sys%size
            IF (ABS(sys%equations(ri, r)) .GT.
     &          ABS(sys%equations(peak, r))) peak = ri
          END DO

* No clue.
          IF (ABS(sys%equations(peak,r)) < EPSILON) THEN
            error = .TRUE.
            RETURN
          END IF

* If peak row found, swap into pivot position.
          IF (peak .NE. r) THEN
*   Using augmented matrix, solution is the final column.
            temprow = sys%equations(peak,:)
            sys%equations(peak, :) = sys%equations(r, :)
            sys%equations(r, :) = temprow
          END IF

* Eliminate variable in all other rows.
          DO ri = 1, sys%size
            IF (ri .NE. r) sys%equations(ri,:) = sys%equations(ri,:) + 
     & (sys%equations(r,:) * (-sys%equations(ri,r)/sys%equations(r,r)))
          END DO
        END DO

        DO r = 1, sys%size
          sys%equations(r,sys%size+1) = 
     &    sys%equations(r,sys%size+1) / sys%equations(r,r)
          sys%equations(r,r) = 1
        END DO

        error = .FALSE.
      END SUBROUTINE

      END PROGRAM
