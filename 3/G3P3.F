
* Validating filenames.
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
        END SUBROUTINE f_flags

* True returned for filenames corresponding 'valid' files (validity dependent on
* i or o). Sets flags to reflect the state of the file.
        LOGICAL FUNCTION valid_ifile(filename)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: filename

          CALL f_flags(filename)
          valid_ifile = f_exists.AND..NOT.(f_isopen.OR.f_forbidden)
        END FUNCTION valid_ifile
        
        LOGICAL FUNCTION valid_ofile(filename)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: filename

          CALL f_flags(filename)
          valid_ofile = (.NOT. f_exists) .AND. 
     &                   .NOT. (f_isopen .OR. f_forbidden)
        END FUNCTION valid_ofile
      END MODULE file_check

* * Prompting for filenames / prompt loops for valid filenames.
*       MODULE file_prompt
*         USE files
* 
*         IMPLICIT NONE
*         INTEGER, PARAMETER :: MAX_INPUT_LENGTH = MAX_FILENAME_LENGTH
*         
* * Signals a quit choice.
*         LOGICAL :: abort
* 
*         CONTAINS
* 
* * Bad to use assumed length for OUT string here?
*         SUBROUTINE prompt_str (prompt, str) 
*           IMPLICIT NONE 
*           CHARACTER(len=*), INTENT(IN) :: prompt
*           CHARACTER(len=MAX_INPUT_LENGTH), INTENT(OUT) :: str
* 100       FORMAT (' ', T1, A)
* 200       FORMAT (A)
*           
*           str = ''
*           WRITE (*, 100, advance='no') prompt
*           READ (*, 200, iostat=ios, iomsg=iom) str
*           str = trim(str)
*         END SUBROUTINE prompt_str
* 
*         CHARACTER FUNCTION prompt (msg) 
*           IMPLICIT NONE 
*           CHARACTER(len=*), INTENT(IN) :: msg
* 100       FORMAT (' ', T1, A)
* 200       FORMAT (A)
*           WRITE (*, 100, advance='no') msg
*           READ (*, 200) prompt
*         END FUNCTION
* 
*         SUBROUTINE prompt_ifile(fn)
*           IMPLICIT NONE
*           CHARACTER(len=MAX_INPUT_LENGTH), INTENT(OUT) :: fn
* 
* 10        FORMAT (' ', T1, "ERR: File '", A, "' doesn't exist!")
* 20        FORMAT ("ERR: File '", A, "' is forbidden!")
* 30        FORMAT ("ERR: File '", A, "' is already open!")
* 
*           CALL prompt_str("Enter the input file: ", fn)
*           
*           DO WHILE (.NOT. (valid_ifile(fn) .OR. abort))
*             IF (f_forbidden) THEN
*               WRITE (*, 20) trim(fn)
*               CALL prompt_str("Enter a different input file: ", fn)
*             ELSE IF (.NOT. f_exists) THEN 
*               WRITE (*, 10) trim(fn)
*               CALL prompt_str("Enter another filename or " //
*      &                        "type 'QUIT' to quit: ", fn)
*               abort = trim(fn) .EQ. "QUIT"
*             ELSE IF (f_isopen) THEN
*               WRITE (*, 30) trim(fn)
*               CALL prompt_str("Enter a different input file: ", fn)
*             END IF
*           END DO
*         END SUBROUTINE
*         
*         SUBROUTINE prompt_ofile(fn)
*           IMPLICIT NONE
*           CHARACTER(len=*), INTENT(OUT) :: fn
*           LOGICAL :: overwrite
* 
*           overwrite = .FALSE.
* 
* 11        FORMAT (' ', T1, "WARN: File '", A, "' exists...")
* 20        FORMAT ("ERR: File '", A, "' is forbidden!")
* 30        FORMAT ("ERR: File '", A, "' is already open!")
* 
*           CALL prompt_str("Enter the output file: ", fn)
*           
*           DO WHILE (.NOT. (valid_ofile(fn, overwrite) .OR. abort))
*             IF (f_forbidden) THEN
*               WRITE (*, 20) trim(fn)
*               CALL prompt_str("Enter a different output file: ", fn)
*             ELSE IF (f_exists) THEN 
*               WRITE (*, 11) trim(fn)
*               CALL prompt_str("Enter OVERWRITE to overwrite, QUIT " //
*      &                        "to quit, or a different filename: ", fn)
*               abort = trim(fn) .EQ. "QUIT"
*               overwrite = trim(fn) .EQ. "OVERWRITE"
*             ELSE IF (f_isopen) THEN
*               WRITE (*, 30) trim(fn)
*               CALL prompt_str("Enter a different output file: ", fn)
*             END IF
*           END DO
*         END SUBROUTINE
*       END MODULE prompts

* Shorthand for shape array access.
#define COL 1
#define ROW 2
#define MAT 3
      MODULE sum_matrices 
        IMPLICIT NONE
        INTEGER, PARAMETER :: MAX_COLS=10, MAX_ROWS=10, MAX_MATS=10
        
* Flat memory space for matrices. 
* Dimensionality unknown until runtime. Read into 'aspect'.
* Subroutines interpret 'matrices' tensor shape using 'aspect'.
        REAL :: matrices(MAX_COLS*MAX_ROWS*MAX_MATS)
        REAL :: sum_matrix(MAX_COLS*MAX_ROWS)
        INTEGER :: aspect(3)

        CONTAINS

        SUBROUTINE read_matrices (matrices, aspect)
          INTEGER, INTENT(IN) :: aspect(3)
          REAL, INTENT(OUT) :: matrices(aspect(COL), aspect(ROW), aspect(MAT))

          INTEGER :: c, i, r

          DO i = 1, aspect(MAT) 
            DO r = 1, aspect(ROW)
              PRINT *, (matrices(c,r,i), c = 1, aspect(COL))
            END DO
          END DO

*          matrices = reshape([(i, i = 1, 800)], [count, cols, rows])
*          print *, ((matrices(1,c,r), c = 1, cols), r = 1, rows)
        END SUBROUTINE
      END MODULE
#undef COL
#undef ROW
#undef MAT

      PROGRAM main
        USE file_check
*        USE file_prompter
*        USE matrix_module

        IMPLICIT NONE
        INTEGER, PARAMETER :: INPUT_LENGTH = MAX_FILENAME_LENGTH

        LOGICAL :: running
        INTEGER :: state
        INTEGER :: ifile_unit, ofile_unit
        
        INTEGER :: ios
        CHARACTER(len=256) :: iom
        CHARACTER(len=INPUT_LENGTH) :: input

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
          input = prompt(
     &    "Enter a different output file, type 'OVERWRITE' "//
     &    "to overwrite '"//trim(input)//"', or type 'QUIT' to quit: ")
          IF (trim(input) .EQ. "QUIT") THEN 
            state = 0
          ELSE IF (trim(input) .EQ. "OVERWRITE") THEN
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
          print *, "read i"
          state = 4
* 9 Open output file.
        CASE (9)
          print *, "read o"
          state = 10
* 10 Do the program.
        CASE (10)
          print *, "matrices"
          state = 11
* 11 Close files and go away.
        CASE (11)
          print *, "file closes"
          state = 0
        END SELECT
        END DO


        CONTAINS 

        CHARACTER(len=INPUT_LENGTH) FUNCTION prompt (msg) 
          IMPLICIT NONE 
          CHARACTER(len=*), INTENT(IN) :: msg
*          CHARACTER(len=INPUT_LENGTH), INTENT(OUT) :: prompt
100       FORMAT (' ', T1, A)
200       FORMAT (A)
          WRITE (*, 100, advance='no') msg
          READ (*, 200) prompt
        END FUNCTION
          
              
*        CALL prompt_ifile(input)
*        IF (.NOT. abort) STOP

C        INTEGER :: ios
C        CHARACTER(len=256) :: iom 
C
C
C        input = '' 
C        quitter = .FALSE.
C        overwrite = .FALSE.
C
C        CALL prompt_ifile(input, quitter)
C        print *, input, quitter
C        IF (quitter) STOP
C
C        OPEN (file=input, newunit=if_unit, action='READ', 
C     &  status='OLD', iostat=ios)
C
C        DO WHILE (ios .EQ. 0)
C        read (if_unit, '(A)', iostat=ios) input
C        print *, TRIM(input)
C        END DO
C        print *, ios
C
C        STOP
C        
C        CALL prompt_ofile(input, quitter)
C        print *, input, quitter
C        IF (quitter) STOP
C        
C        print *, "DONE!"
C        
*         matrices = 0
*         aspect(COLS) = 10
*         aspect(ROWS) = 10
*         aspect(MATS) = 10
*         
*        PRINT *, "test"
*        PRINT *, shape(matrices), size(matrices)
*        PRINT *, shape(sum_matrix), size(sum_matrix)
*        PRINT *, aspect
*        print *, MAX_COLS, MAX_ROWS, MAX_MATS
*
*        matrices = [(i, i = 1, product(aspect))]
*
*        CALL read_matrices(matrices, aspect)
*
*        CALL prompt_filename(infile, "THE NAME?! ")
*
*        print *, TRIM(infile), TRIM(outfile)
*
*        CALL prompt_fopen ("test exists: ", ifunit, .FALSE.)

*        call mstuff (m, mshape)
*        call mstuff(matrices, cols, rows)
      END PROGRAM main

