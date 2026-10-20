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
                WRITE (*, '(A, A, A)') "Overwriting '", trim(fn), "'."
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
      IMPLICIT NONE
      INTEGER :: u1, u2
      LOGICAL :: bust

      CALL ifileorbust (u1, bust)
      print *, bust
      if (.not. bust) close(u1)
      CALL ofileorbust (u2, bust)
      print *, bust
      if (.not. bust) close(u2)

      END PROGRAM
