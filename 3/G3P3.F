****************************************
* Program 2: Matrix Add
* Group 3
*
*  Cody Baker 
*  Mikiya Dixon
*  Ravi Dressler
****************************************

* Shorthand for shape array access.
#define COL 1
#define ROW 2
#define MAT 3

* *
* Validating filenames.
* *
      MODULE file_check
        IMPLICIT NONE 
        INTEGER, PARAMETER :: MAX_FILENAME_LENGTH = 255
        CHARACTER(len=2), PARAMETER :: FORBIDDEN_FILES(2) = ['. ', '..']

* Flags set by last checked filename.
        LOGICAL :: f_exists, f_isopen, f_forbidden
      
        CONTAINS 
          
        SUBROUTINE f_flags(filename) 
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: filename

          INQUIRE (file=trim(filename), exist=f_exists, opened=f_isopen)
          f_forbidden = any(FORBIDDEN_FILES .EQ. trim(filename))
        END SUBROUTINE

* True returned for filenames pointing to 'valid' files (validity is 
* different for i vs o). Three flags also signal the state of the file.
        LOGICAL FUNCTION valid_ifile(filename)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: filename

          CALL f_flags(filename)
          valid_ifile = f_exists.AND..NOT.(f_isopen.OR.f_forbidden)
        END FUNCTION
        
        LOGICAL FUNCTION valid_ofile(filename)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: filename

          CALL f_flags(filename)
          valid_ofile = (.NOT. f_exists) .AND. 
     &                   .NOT. (f_isopen .OR. f_forbidden)
        END FUNCTION
      END MODULE

* *
* Reading matrices, summing, writing matrices.
* * 
      MODULE matrix_add
        IMPLICIT NONE
        INTEGER, PARAMETER :: MAX_COLS=10, MAX_ROWS=10, MAX_MATS=10

        CONTAINS

        SUBROUTINE read_aspect (aspect, unit, ios, iom)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: unit
          INTEGER, INTENT(OUT) :: ios
          CHARACTER(len=256), INTENT(OUT) :: iom

          INTEGER, INTENT(OUT) :: aspect(3)

          READ (unit, *, iostat=ios, iomsg=iom) 
     &    aspect(MAT), aspect(ROW), aspect(COL)

*   Restrict dimensions to max parameters.
          aspect(COL) = clamp(aspect(COL), 1, MAX_COLS)
          aspect(ROW) = clamp(aspect(ROW), 1, MAX_ROWS)
          aspect(MAT) = clamp(aspect(MAT), 1, MAX_MATS)
          
        END SUBROUTINE

        SUBROUTINE read_matrices (matrices, aspect, unit, ios, iom)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: unit
          INTEGER, INTENT(IN) :: aspect(3)
          INTEGER, INTENT(OUT) :: ios
          CHARACTER(len=256), INTENT(OUT) :: iom
          
          REAL, INTENT(OUT) :: matrices(aspect(COL), aspect(ROW), aspect(MAT))
          
          INTEGER :: c, r, m

*   Elements are space delimited, list directed input should work fine.
          READ (unit, *, iostat=ios, iomsg=iom) (((matrices(c, r, m), 
     & c = 1, aspect(COL)), r = 1, aspect(ROW)), m = 1, aspect(MAT))
        END SUBROUTINE

        SUBROUTINE write_matrix (unit, title, matrix, cols, rows)
          INTEGER, INTENT(IN) :: unit
          INTEGER, INTENT(IN) :: rows, cols
          REAL, INTENT(IN) :: matrix(cols, rows)
          CHARACTER(len=*), INTENT(IN) :: title
          
          INTEGER :: r, c
          
          WRITE (*, *) trim(title)
          WRITE (unit, *) trim(title)
          DO r = 1, rows
            WRITE (*, *) (matrix(c, r), c = 1, cols)
            WRITE (unit, *) (matrix(c, r), c = 1, cols)
          END DO
        END SUBROUTINE

        SUBROUTINE write_matrices (unit, matrices, aspect)
          INTEGER, INTENT(IN) :: unit
          INTEGER, INTENT(IN) :: aspect(3)
          REAL, INTENT(IN) :: matrices(aspect(COL), aspect(ROW), aspect(MAT))
          
          INTEGER :: r, c, m
          CHARACTER(len=32) title

100       FORMAT ("MATRIX ", I2)
          DO m = 1, aspect(MAT)
            WRITE (title, 100) m
            CALL write_matrix (unit, title, matrices(1, 1, m), 
     &                         aspect(COL), aspect(ROW))
          END DO
        END SUBROUTINE

        SUBROUTINE sum_matrices (sum_matrix, matrices, aspect)
          INTEGER, INTENT(IN) :: aspect(3)
          REAL, INTENT(IN) :: matrices(aspect(COL), aspect(ROW), aspect(MAT))
          REAL, INTENT(OUT) :: sum_matrix(aspect(COL), aspect(ROW))

*   Sum across depth.
          sum_matrix = sum(matrices, dim=3)
        END SUBROUTINE

        INTEGER FUNCTION clamp(val, mi, ma)
          INTEGER, INTENT(IN) :: val
          INTEGER, INTENT(IN) :: mi, ma
          clamp = min(max(val, mi), ma)
        END FUNCTION
      END MODULE

* *
* Prompt for files, check files, read and write matrices.
* *
      PROGRAM main
        USE file_check
        USE matrix_add
        
        IMPLICIT NONE
        INTEGER, PARAMETER :: INPUT_LENGTH = MAX_FILENAME_LENGTH

        LOGICAL :: running
        INTEGER :: state
        INTEGER :: ifile_unit, ofile_unit
        
        INTEGER :: ios
        CHARACTER(len=256) :: iom
        CHARACTER(len=INPUT_LENGTH) :: input, tempstr
        
* Flat array space for matrices. 
* Dimensionality is unknown until runtime, will be stored into 'aspect'.
* Subroutines interpret the shape of 'matrices' memory using 'aspect'.
        INTEGER :: aspect(3)
        REAL :: matrices(MAX_COLS*MAX_ROWS*MAX_MATS)
        REAL :: sum_matrix(MAX_COLS*MAX_ROWS)

        aspect = 0
        matrices = 0
        sum_matrix = 0

* Run loop. 
* Program is structured in states.
        state = 1
        running = .TRUE.
        DO WHILE (running)
        SELECT CASE (state)

* 0 Close program.
        CASE (0)
          WRITE (*, *) "Goodbye!"
          running = .FALSE.

* 1 Input file prompt, first pass.
        CASE (1)
          input = prompt("Enter an input file: ")
          state = 3

* 2 Input file prompt, second pass.
        CASE (2)
          input = prompt(
     &    "Enter an different input file, or type 'QUIT' to quit: ")
          IF (trim(input) .EQ. "QUIT") THEN 
            state = 0
          ELSE
            state = 3
          END IF

* 3 Input file name validation.
        CASE (3)
          IF (valid_ifile(input)) THEN
            state = 8
          ELSE 
            IF (f_forbidden) THEN
              WRITE (*, *) "File '"//trim(input)//"' is forbidden!"
            ELSE IF (.NOT. f_exists) THEN
              WRITE (*, *) "File '"//trim(input)//"' does not exist!"
            ELSE IF (f_isopen) THEN
              WRITE (*, *) "File '"//trim(input)//"' is in use!"
            END IF
            state = 2
          END IF

* 4 Output file prompt, first pass.
        CASE (4)
          input = prompt("Enter an output file: ")
          state = 7

* 5 Output file prompt, second pass.
        CASE (5)
          input = prompt(
     &    "Enter a different output file, or type 'QUIT' to quit: ")
          IF (trim(input) .EQ. "QUIT") THEN 
            state = 0
          ELSE
            state = 7
          END IF

* 6 Output file prompt, overwrite case.
        CASE (6)
          tempstr = input
          input = prompt(
     &    "Enter a different output file, type 'OVERWRITE' "//
     &    "to overwrite '"//trim(input)//"', or type 'QUIT' to quit: ")
          IF (trim(input) .EQ. "QUIT") THEN 
            state = 0
          ELSE IF (trim(input) .EQ. "OVERWRITE") THEN
            input = tempstr
            state = 9
          ELSE
            state = 7
          END IF

* 7 Output file name validation.
        CASE (7)
          IF (valid_ofile(input)) THEN
            state = 9
          ELSE 
            IF (f_forbidden) THEN
              WRITE (*, *) "File '"//trim(input)//"' is forbidden!"
              state = 5
            ELSE IF (f_isopen) THEN
              WRITE (*, *) "File '"//trim(input)//"' is in use!"
              state = 5
            ELSE IF (f_exists) THEN
              WRITE (*, *) "File '"//trim(input)//"' already exists!"
              state = 6
            END IF
          END IF

* 8 Open input file.
        CASE (8)
          OPEN (file=input, newunit=ifile_unit, action="READ", 
     & status="OLD", iostat=ios, iomsg=iom)
          IF (ios .NE. 0) THEN
            WRITE (*, *) "Error opening file: '"//trim(iom)//"'", ios
            IF (prompt_retry("Try another file?")) THEN
              state = 1
            ELSE 
              state = 0
            END IF
          ELSE
            WRITE (*, *) "Input file opened successfully."
            state = 4
          END IF

* 9 Open output file.
        CASE (9)
          OPEN (file=input, newunit=ofile_unit, action="WRITE", 
     & status="REPLACE", iostat=ios, iomsg=iom)
          IF (ios .NE. 0) THEN
            WRITE (*, *) "Error opening file: '"//trim(iom)//"'", ios
            IF (prompt_retry("Try another file?")) THEN
              state = 4
            ELSE 
*            This state shouldn't be reachable without an open in file.
*            If quitting, be kind, and close it.
              CLOSE (ifile_unit)
              state = 0
            END IF
          ELSE
            WRITE (*, *) "Output file opened successfully."
            state = 10
          END IF

* 10 Do the program.
        CASE (10)
*   Read from input.
          CALL read_aspect(aspect, ifile_unit, ios, iom)
*       IOS are most likely EOF errors. Just warn and continue, probably fine.
          IF (ios .NE. 0) WRITE (*, *) WRITE_chain(ofile_unit,  
     &      "Warning: IO error while reading dimensions "//
     &      "(Is your file formatted?): '"//trim(iom)//"'") 
          CALL read_matrices(matrices, aspect, ifile_unit, ios, iom)
          IF (ios .NE. 0) WRITE (*, *) WRITE_chain(ofile_unit,
     &      "Warning: IO error while reading matrices "//
     &      "(Are your dimensions right?): '"//trim(iom)//"'")

          CALL sum_matrices(sum_matrix, matrices, aspect)

*   Write to output.          
          print *, ''
          CALL write_matrices(ofile_unit, matrices, aspect)
100       FORMAT ("SUM OF ALL ", I2, " MATRIX ", I2)
          WRITE (tempstr, 100) aspect(MAT), aspect(MAT)+1
          CALL write_matrix(ofile_unit, tempstr, sum_matrix,
     &                      aspect(COL), aspect(ROW))
          print *, ''

          state = 11

* 11 Close files and go away.
        CASE (11)
          WRITE (*, *) "Closing output file."
          CLOSE (ofile_unit)
          WRITE (*, *) "Closing input file."
          CLOSE (ifile_unit)
          state = 0
        END SELECT
        END DO

        CONTAINS 

        CHARACTER(len=INPUT_LENGTH) FUNCTION prompt(msg) 
          IMPLICIT NONE 
          CHARACTER(len=*), INTENT(IN) :: msg
100       FORMAT (' ', T1, A)
200       FORMAT (A)
          WRITE (*, 100, advance='no') msg
          READ (*, 200) prompt
        END FUNCTION

        LOGICAL FUNCTION prompt_retry(msg)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: msg
          CHARACTER :: retry
          
          retry = trim(prompt((msg//" (y/n): ")))
          DO WHILE (.NOT. any(retry .EQ. ["Y", "y", "N", "n"]))
            retry = trim(prompt("Y or N: "))
          END DO

          prompt_retry = any(retry .EQ. ["Y", "y"])
        END FUNCTION

        CHARACTER(len=255) FUNCTION WRITE_chain(unit, msg)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: unit
            CHARACTER(len=*), INTENT(IN) :: msg
            WRITE (unit, *) msg
            WRITE_chain = msg
        END FUNCTION
      END PROGRAM main

