****************************************
* Program 5: Giraffer
* Group 3
*
*  Cody Baker 
*  Mikiya Dixon
*  Ravi Dressler
****************************************

      MODULE fileprompts
        CHARACTER(len=256) :: fn
        CHARACTER(len=256) :: iom
        INTEGER :: ios
      
        CONTAINS

* Prompt
* Inquire about existence
* If no good, reprompt and cycle
* Try to open up
* If no good, reprompt and cycle
        SUBROUTINE ifileorbust(unit, bust)
          IMPLICIT NONE
          INTEGER, INTENT(OUT) :: unit
          LOGICAL, INTENT(OUT) :: bust
          LOGICAL :: nofile, exists

          bust = .FALSE.
          nofile = .TRUE.
          WRITE (*, '(A)', advance='no') "Enter input file: "
          READ (*, '(A)') fn
          DO WHILE (nofile .AND. .NOT. bust)

            INQUIRE (file=trim(fn), exist=exists)
            IF (.NOT. exists) THEN
              WRITE (*, '(A)', advance='no') 
     &      "File does not exist, try again or type QUIT to quit: "
              READ (*, '(A)') fn
              bust = (trim(fn) .EQ. "QUIT")
              CYCLE
            END IF

            OPEN (file=fn, newunit=unit, action="READ", status="OLD", 
     &          iostat=ios, iomsg=iom)
            IF (ios .NE. 0) THEN
              WRITE (*, '(A, A)') "IO error: ", iom
              WRITE (*, '(A)', advance='no') 
     &      "Try another file or type QUIT to quit: "
              READ (*, '(A)') fn
              bust = (trim(fn) .EQ. "QUIT")
              CYCLE
            END IF

            nofile = .FALSE.
          END DO
        ENDSUBROUTINE
* todo: could pass functions in to reduce duplicated code 
        SUBROUTINE ofileorbust(unit, bust)
          IMPLICIT NONE
          INTEGER, INTENT(OUT) :: unit
          LOGICAL, INTENT(OUT) :: bust
          LOGICAL :: nofile, exists

          bust = .FALSE.
          nofile = .TRUE.
          WRITE (*, '(A)', advance='no') "Enter output file: "
          READ (*, '(A)') fn
          DO WHILE (nofile .AND. .NOT. bust)
*     Check existence, retry if non.
            INQUIRE (file=trim(fn), exist=exists)
            IF (exists) THEN
              WRITE (*, '(A)', advance='no') 
     &      "File exists, you can OVERWRITE, QUIT, or try again: "
              READ (*, '(A)') fn
              IF (trim(fn) .EQ. "OVERWRITE") THEN
                WRITE (*, '(A)') "Overwriting..."
              ELSE 
                bust = (trim(fn) .EQ. "QUIT")
                CYCLE
              END IF
            END IF
*     Try to open, retry if errors
            OPEN (file=fn, newunit=unit, action="WRITE", 
     &          status="REPLACE", iostat=ios, iomsg=iom)
            IF (ios .NE. 0) THEN
              WRITE (*, '(A, A)') "IO error: ", iom
              WRITE (*, '(A)', advance='no') 
     &      "Try another file or type QUIT to quit: "
              READ (*, '(A)') fn
              bust = (trim(fn) .EQ. "QUIT")
              CYCLE
            END IF
            nofile = .FALSE.
          END DO
        ENDSUBROUTINE

      END MODULE

      MODULE pointstuff
        INTEGER, PARAMETER :: X=1, Y=2
        INTEGER, PARAMETER :: POINT_SIZE=2
        INTEGER, PARAMETER :: MAX_POINTS=200
        INTEGER :: ios

        CONTAINS

        SUBROUTINE read_points(unit, points, points_n, fail) 
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: unit
          INTEGER, INTENT(OUT) :: points_n
          LOGICAL, INTENT(OUT) :: fail
          REAL, INTENT(OUT) :: points(POINT_SIZE * MAX_POINTS)
          INTEGER :: i

          READ (unit, *, iostat=ios) points_n
* Checking on size header. Change points_n or abort if unrecoverable.
* Follow are formatted in pairs, flat read will store X, Y, X, Y contiguosly.
          READ (unit, *, iostat=ios) points
* Watch out for EOF. Count points anyways and fix points_n if mismatch.
        END SUBROUTINE

        SUBROUTINE write_points(unit, points, points_n)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: unit
          INTEGER, INTENT(IN) :: points_n
          REAL, INTENT(IN) :: points(POINT_SIZE, points_n)
          INTEGER :: p
         
          DO p = 1, points_n 
            WRITE (unit, *, iostat=ios) points(:,p)
          END DO
        END SUBROUTINE

        SUBROUTINE sort_points(points, points_n, by)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: points_n, by
* Move POINT_SIZE sized blocks to preserve pairings.
          REAL, INTENT(INOUT) :: points(POINT_SIZE, points_n)
          INTEGER :: i, j
          REAL :: swap(POINT_SIZE)

* 'I Can't Believe It Can Sort' sort.
* https://arxiv.org/pdf/2110.01111
          DO i = 1, points_n
            DO j = 1, points_n
*   Points stored in columns.
              IF (points(by, i) .LT. points(by, j)) THEN
                swap = points(:,i)
                points(:,i) = points(:,j)
                points(:,j) = swap
              END IF
            END DO
          END DO
        END SUBROUTINE
      END MODULE

*      MODULE grapher 
*
*        SUBROUTINE graph_set_points(points, points_n)
*        END SUBROUTINE
*        
*        SUBROUTINE graph_write_info(unit)
*        END SUBROUTINE
*
*        SUBROUTINE graph_write_graph(unit)
*        END SUBROUTINE
*
*      END MODULE

      PROGRAM main
* const MAX_POINTS, Y_COL_LENGTH, X_ROW_RANGE
* const X=1,Y=2
*
* CALL iopener (iunit, ounit, quitter)
*  icheck (filename)
*  ocheck (filename)
*  INQUIRE (filename)
* IF (quitter) stop or smthn
* CALL ireader (iunit, points, points_n, quitter)
*  - read x + y into points in two rows. Make pairs contiguous for sorting.
*  - handle mismatch point count, abort if unrecoverable (0 or -).
* 
* IF (quitter) stop or smthn (could do loop w/ case)
* ranges(2:2) = ([minval(points(i,:)), 
*                 maxval(points(i,:))], i = X, Y)
* 
* WRITE 
*
        USE fileprompts
        USE pointstuff
        IMPLICIT NONE

        INTEGER :: points_n
        REAL, DIMENSION(POINT_SIZE, MAX_POINTS) :: points
        REAL, DIMENSION(2, POINT_SIZE) :: ranges
        
        INTEGER :: u1, u2
        INTEGER :: p
        LOGICAL :: bust

        INTEGER :: plotlines
        REAL :: y_step, x_step

        CALL ifileorbust (u1, bust)
        CALL read_points (u1, points, points_n, bust)
        CLOSE (u1)
        DO p = 1, points_n 
          WRITE (*, *) points(:,p)
        END DO

        print *, ""

        CALL sort_points (points, points_n, X)
        DO p = 1, points_n 
          WRITE (*, *) points(:,p)
        END DO
        
        
*        IF (.NOT. bust) THEN
*          CALL ofileorbust (u2, bust)
*
*          IF (.NOT. bust) THEN
*            CALL read_points (u1, points, points_n, bust)
*            CLOSE (u1)
*
*            IF (.NOT. bust) THEN
*              WRITE (*, '(A)') "DATA POINTS"
*              CALL write_points (u2, points, points_n)
*              WRITE (*, '(A)') "SORTED DATA"
*              CALL sort_points (points, points_n)
*              CALL write_points (u2, points, points_n) 
*
*              CALL graph_points (u2, points, points_n)
*              CLOSE (u2)
*            END IF
*          END IF
*        END IF
       
        WRITE (*, '(A)') "Goodbye!"
      END PROGRAM
