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

* Check against:
*  empty file EOF
*  out of bounds point count
*  premature point read EOF
          READ (unit, *, iostat=ios) points_n
          IF (ios .NE. 0) THEN
            WRITE (*, *) "Could not parse point count from file!"
            FAIL = .TRUE.
          ELSE IF (points_n .LT. 1 .OR. points_n .GT. MAX_POINTS) THEN
            WRITE (*, '(A,"(",I3," -> ",I3,")")') 
     &       "Point count header is out of bounds! ", 1, MAX_POINTS
            FAIL = .TRUE.
          ELSE
            READ (unit, *, iostat=ios) points(1:POINT_SIZE*points_n)
            IF (ios .NE. 0) THEN
              WRITE (*, '(A, I3, A)') "Could not read all points!" // 
     &         "Is your point count header (", points_n, ") accurate?"
              FAIL = .TRUE.
            END IF
          END IF
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
          REAL, INTENT(INOUT) :: points(POINT_SIZE, points_n)
          INTEGER :: i, j
          REAL :: swap(POINT_SIZE)

* 'I Can't Believe It Can Sort' sort.
* https://arxiv.org/pdf/2110.01111
          DO i = 1, points_n
            DO j = 1, points_n
*   Swap whole columns, (points stored as columns).
              IF (points(by, i) .LT. points(by, j)) THEN
                swap = points(:,i)
                points(:,i) = points(:,j)
                points(:,j) = swap
              END IF
            END DO
          END DO
        END SUBROUTINE
      END MODULE

      MODULE grapher 
        INTEGER, PARAMETER :: X=1, Y=2


* Format 10.4, 15 columns + 1 sep
        INTEGER, PARAMETER :: YLABEL_COL_WIDTH=16
        INTEGER, PARAMETER :: XLABEL_COL_WIDTH=16, XLABEL_ROW_SPACE=4
        INTEGER, PARAMETER :: MIN_VALUES=20, MAX_VALUES=200
        INTEGER, PARAMETER :: COLUMNS=80

        INTEGER, PARAMETER :: YLABELS =
     &  (COLUMNS - XLABEL_COL_WIDTH) / YLABEL_COL_WIDTH

        REAL, DIMENSION(:,:), POINTER :: points
        INTEGER :: points_n
        REAL :: ranges(2, 2)
        REAL :: step(2)
        INTEGER :: xvalues
        REAL :: dx

        CONTAINS

        SUBROUTINE graph_set_points(ps, ps_n)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: ps_n
          REAL, TARGET, INTENT(IN) :: ps(2, ps_n)
          INTEGER :: i
          REAL :: dxtmp

          points => ps
          points_n = ps_n

          ranges(X,:) = [minval(points(X,:)), maxval(points(X,:))]
          ranges(Y,:) = [minval(points(Y,:)), maxval(points(Y,:))]

          step(Y) = (ranges(Y, 2) - ranges(Y, 1)) / YLABELS
          
          dx = ranges(X, 2) - ranges(X, 1)
          DO i = 2, points_n
            dxtmp = points(X, i) - points(X, i-1)
            IF (dxtmp .NE. 0 .AND. dxtmp .LT. dx) dx = dxtmp
          END DO

          step(X) = 0
          IF (dx .GT. 0) THEN 
            step(X) = dx
            xvalues = (ranges(X, 2) - ranges(X, 1)) / dx + 1
            IF (xvalues < MIN_VALUES) THEN
              xvalues = MIN_VALUES
              step(X) = (ranges(X, 2) - ranges(X, 1)) / (MIN_VALUES-1)
            END IF
          END IF

        END SUBROUTINE
        
        SUBROUTINE graph_write_info(unit)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: unit
          INTEGER i

*          WRITE (*, '(3(X,X,X,X, A6, F10.4))') 
          WRITE (*, *) 
     & "YMIN: ", ranges(Y,1) ,"YMAX: ", ranges(Y,2) ,"YSTEP: ", step(Y)
          WRITE (*, *)
     & "XMIN: ", ranges(X,1) ,"XMAX: ", ranges(X,2) ,"dx: ", step(X),
     & "xvalues: ", xvalues 

          PRINT *, [(points(X,1)+step(X)*i, i=0, xvalues-1)]
        END SUBROUTINE

*        SUBROUTINE graph_write_graph(unit)
*        END SUBROUTINE
*
      END MODULE

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
        USE grapher
        IMPLICIT NONE

        INTEGER :: ps_n
        REAL, DIMENSION(POINT_SIZE, MAX_POINTS), TARGET :: ps
        
        INTEGER :: u1, u2
        INTEGER :: p
        LOGICAL :: bust

        INTEGER :: plotlines
        REAL :: y_step, x_step

        CALL ifileorbust (u1, bust)
        CALL read_points (u1, ps, ps_n, bust)
        CLOSE (u1)
        DO p = 1, ps_n 
          WRITE (*, *) ps(:,p)
        END DO
        
        CALL sort_points (ps, ps_n, 1)

        print *, ""

*        CALL sort_points (ps, ps_n, X)
        DO p = 1, ps_n 
          WRITE (*, *) ps(:,p)
        END DO
        
        print *, ""

        CALL graph_set_points (ps, ps_n)
        CALL graph_write_info (1)
        
        
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
