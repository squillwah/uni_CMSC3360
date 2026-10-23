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

* Format 10.4, 10 columns + 1 y sep '|' and 2 x sep '-='
        INTEGER, PARAMETER :: YLABEL_COL_WIDTH=11
        INTEGER, PARAMETER :: XLABEL_COL_WIDTH=12, XLABEL_LINE_SPACE=4
        INTEGER, PARAMETER :: COLUMNS=80

* Rounded down. 
        INTEGER, PARAMETER :: YCOLUMNS=(COLUMNS - XLABEL_COL_WIDTH)
* Labels start left of their place, so full columns are used. -2 for Y>
        INTEGER, PARAMETER :: YLABELS=((COLUMNS-2) / YLABEL_COL_WIDTH)

* It would be great to use the preprocessor for this.
*        CHARACTER(len=*) :: FORMAT_YLINE_LABEL = 
*     &  "('Y>', "//YLABELS//"(F10.4, '|'))"
*        CHARACTER(len=*) :: FORMAT_YLINE_SPACE =
*     &  "('vX', "//YLABELS//"(' - - - - - - - ='))"
*        CHARACTER(len=*) :: FORMAT_XLINE_LABEL =
*     &  "(F10.4, '|=', "//YCOLUMNS//"A)"
*        CHARACTER(len=*) :: FORMAT_XLINE_SPACE =
*     &  "(15('.'), '|=', "//YCOLUMNS//"A)"
        
        INTEGER, PARAMETER :: MIN_XLINES=20, MAX_XLINES=200
        INTEGER :: xlines, xlabels
      
        REAL :: ranges(2, 2)
        REAL :: dxy(2)
        
        REAL, DIMENSION(:,:), POINTER :: points
        INTEGER :: points_n

* 1. Store count at each point quantum, 2. render symbols to line, 3. print.
        INTEGER :: p_density(YCOLUMNS)
        CHARACTER :: p_symbols(4) = ['.', '*', '+', '#']
        CHARACTER :: g_line(YCOLUMNS)

        CONTAINS

        SUBROUTINE graph_set_points(ps, ps_n)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: ps_n
          REAL, TARGET, INTENT(IN) :: ps(2, ps_n)
          INTEGER :: i
          REAL :: dxtmp

* Y>12345.1234|12345.1234|
* vX----------=----------=
* 12345.1234|=0123456789a 0 + 10 dy steps between labels
* ..........|-1
* ..........|-2
* ..........|-3
* ..........|-4
* ..........|-5    0 + 5 dx steps between labels
* 12345.1234|=

          points => ps
          points_n = ps_n

          ranges(X,:) = [minval(points(X,:)), maxval(points(X,:))]
          ranges(Y,:) = [minval(points(Y,:)), maxval(points(Y,:))]

* Change in X per line (line count derived from x values and min/max bounds).
          dxy(X) = ranges(X, 2) - ranges(X, 1)
          DO i = 2, points_n
            dxtmp = points(X, i) - points(X, i-1)
            IF (dxtmp .NE. 0 .AND. dxtmp .LT. dxy(X)) dxy(X) = dxtmp
          END DO

* Set xlines by dx. If outside MIN and MAX, constrain and adjust dx to fit.
          xlines = MIN_XLINES
          IF (dxy(X) .GT. 0) THEN 
            xlines = (ranges(X, 2) - ranges(X, 1)) / dxy(X) + 1
            IF (xlines < MIN_XLINES) THEN
              xlines = MIN_XLINES
              dxy(X) = (ranges(X, 2) - ranges(X, 1)) / (MIN_XLINES-1)
            ELSE IF (xlines > MAX_XLINES) THEN
              xlines = MAX_XLINES
              dxy(X) = (ranges(X, 2) - ranges(X, 1)) / (MAX_XLINES-1)
            END IF
          END IF
          
* Change in Y per column (column count is known and constant). 
* The -1 is needed, otherwise largest point is missed, for some reason.
          dxy(Y) = (ranges(Y, 2) - ranges(Y, 1)) / (YCOLUMNS-1)
        END SUBROUTINE
        
        SUBROUTINE graph_write_info(unit)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: unit
          INTEGER :: i

          WRITE (*, *)
     & "XMIN: ", ranges(X,1) ,"XMAX: ", ranges(X,2) ,"dx: ", dxy(X),
     & "xvalues: ", xlines 
          WRITE (*, *) 
     & "YMIN: ", ranges(Y,1) ,"YMAX: ", ranges(Y,2) ,"dy: ", dxy(Y),
     & "yvalues: ", YCOLUMNS
        END SUBROUTINE

        SUBROUTINE graph_write_plot(unit)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: unit
          CHARACTER(len=64) :: fmt
          INTEGER :: i, line, col
          REAL :: bounds(2)

* Y label header
* + line below, with dashes going past final label.
          WRITE (fmt, '(A, I4, A)') 
     &    "('Y>', ", YLABELS, "(F10.4, '|'))"
          WRITE (*, fmt) 
     &    [(ranges(Y,1)+dxy(Y)*(YLABEL_COL_WIDTH)*i, i=0, YLABELS-1)]

          WRITE (fmt, '(A, I4, A)') 
     &    "('vX', ", YLABELS, "(10('-'), '='))"
          WRITE (*, fmt, advance='no') 
          i = COLUMNS-2-(YLABELS*YLABEL_COL_WIDTH)
          if (i .GT. 0) THEN
            WRITE (fmt, '(A, I4, A)') "(", i, "('-'))"
            WRITE (*, fmt)
          ELSE
            WRITE (*, '("")')
          END IF

* X lines


* Each line is a bound, x+dx*line -> x+dx*(line+1).
* Search through X, plot points into p_density when in bounds, 
* write line and move on when out.
          i = 1
          DO line = 0, xlines-1
            p_density = 1
            bounds = [(points(X,1) + dxy(X)*line), 
     &                (points(X,1) + dxy(X)*(line+1))]

*   Start of lower
            DO WHILE (i .LE. points_n .AND. points(X,i) .LT. bounds(1))
              i = i + 1
            END DO

*   End of upper
            DO WHILE (i .LE. points_n .AND. points(X,i) .LT. bounds(2))
*       Column position is just the amount of dy the origin Y (+1 cause FORTRAN):
              col = INT((points(Y,i)-ranges(Y,1)) / dxy(Y))+1
              p_density(col) = p_density(col) + 1
*              print *, "ROW", line, "COL", col, "VAL", points(X,i),"I",i
              i = i + 1
            END DO

*   Render line 
            DO col = 1, YCOLUMNS
              g_line(col) = p_symbols(min(p_density(col), 
     &                                    size(p_symbols)))
            END DO
            IF (mod(line, XLABEL_LINE_SPACE) .EQ. 0) THEN
              WRITE (fmt, '(A, I4, A)') "(F10.4, '|=', ", YCOLUMNS, "A)"
              WRITE (*, fmt) bounds(1), g_line
            ELSE 
              WRITE (fmt, '(A, I4, A)') "(10X, '|=',", YCOLUMNS, "A)"
              WRITE (*, fmt) g_line
            END IF
          END DO

          

* Store into p_density, for every col on line.
* Render g_line symbols according to p_density.
* Write graph line to screen.

* Between this (x+dx*line) and (x*dx+line+1),
* how many y are between (y+dy*col) and (y+dy*col+1)?

* X is sorted, find subset bound between

*            bounds(2) = [0, 0]
*            i = 1
*            DO WHILE (i.LE.points_n .AND. points(i).LT.xi+dxy(X)*line) 
*              IF i .eq.
*  
*            
*            xi = points(X,1)
*  
*            IF points(X,i) .GE.
*  
*            p_density = 0 
*            DO line = 0, xlines-1
*  * Find bounds of X values inside this line
*              
*              DO i = 1, n_points
*                IF points(X, i) .GE. 
*  
*            g_line = 'x'
* X lines 
*          WRITE (fmt, '(A, I4, A)') "(F10.4, '|=', ", YCOLUMNS, "A)"
*
*          DO i = 1, xlines
*            WRITE (*, fmt) 10.0, g_line
*          END DO


        END SUBROUTINE
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
        CALL graph_write_plot (1)
        
        
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
