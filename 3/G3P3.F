
* Shorthand for shape array access.

* Max filename length in windows.
#define FNLEN 255 

* module to contain
*  iomsg, iostat vars
*  i, c, r (any dummy ints)
*  basic FORMATs

* fopen_prompt(prompt, unit, out)


* vars in module will cause trouble

      MODULE IMPLICIT_SOME
        IMPLICIT NONE
        INTEGER tmp, i, c, r
        INTEGER ios
        LOGICAL tf
        CHARACTER(len=256) iom

        CONTAINS

        SUBROUTINE formats
        END SUBROUTINE formats

      END MODULE IMPLICIT_SOME

      MODULE files
        USE IMPLICIT_SOME
        
        IMPLICIT NONE 
        INTEGER, PARAMETER :: MAX_FILENAME=255
        CHARACTER(len=2), PARAMETER :: FORBIDDEN_FILES(2) = ['. ', '..']

        LOGICAL :: f_exists, f_isopen, f_forbidden
      
        CONTAINS 
          
        SUBROUTINE f_flags(filename) 
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: filename

          INQUIRE (file=trim(filename), exist=f_exists, opened=f_isopen)
          f_forbidden = any(FORBIDDEN_FILES .EQ. trim(filename))
        END SUBROUTINE f_flags

        LOGICAL FUNCTION valid_ifile(filename)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: filename

          CALL f_flags(filename)
          valid_ifile = f_exists.AND..NOT.(f_isopen.OR.f_forbidden)
        END FUNCTION valid_ifile
        
        LOGICAL FUNCTION valid_ofile(filename, overwrite)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: filename
          LOGICAL, INTENT(IN) :: overwrite

          CALL f_flags(filename)
          valid_ofile = (.NOT. f_exists .OR. overwrite) .AND. 
     &                   .NOT. (f_isopen .OR. f_forbidden)
        END FUNCTION valid_ofile
      END MODULE files

      MODULE prompts
        USE files
        CONTAINS

        SUBROUTINE prompt_str (prompt, str) 
          IMPLICIT NONE 
          CHARACTER(len=*), INTENT(IN) :: prompt
          CHARACTER(len=*), INTENT(OUT) :: str
100       FORMAT (' ', T1, A)
200       FORMAT (A)
          
          str = ''
          WRITE (*, 100, advance='no') prompt
          READ (*, 200, iostat=ios, iomsg=iom) str
          str = trim(str)
        END SUBROUTINE prompt_str

        SUBROUTINE prompt_ifile(fn, quitter)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(OUT) :: fn
          LOGICAL, INTENT(OUT) :: quitter

10        FORMAT (' ', T1, "ERR: File '", A, "' doesn't exist!")
20        FORMAT ("ERR: File '", A, "' is forbidden!")
30        FORMAT ("ERR: File '", A, "' is already open!")

          CALL prompt_str("Enter the input file: ", fn)
          
          DO WHILE (.NOT. (valid_ifile(fn) .OR. quitter))
            IF (f_forbidden) THEN
              WRITE (*, 20) trim(fn)
              CALL prompt_str("Enter a different input file: ", fn)
            ELSE IF (.NOT. f_exists) THEN 
              WRITE (*, 10) trim(fn)
              CALL prompt_str("Enter another filename or " //
     &                        "type 'QUIT' to quit: ", fn)
              quitter = trim(fn) .EQ. "QUIT"
            ELSE IF (f_isopen) THEN
              WRITE (*, 30) trim(fn)
              CALL prompt_str("Enter a different input file: ", fn)
            END IF
          END DO
        END SUBROUTINE
        
        SUBROUTINE prompt_ofile(fn, quitter)
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(OUT) :: fn
          LOGICAL, INTENT(OUT) :: quitter
          LOGICAL :: overwrite

          overwrite = .FALSE.

11        FORMAT (' ', T1, "WARN: File '", A, "' exists...")
20        FORMAT ("ERR: File '", A, "' is forbidden!")
30        FORMAT ("ERR: File '", A, "' is already open!")

          CALL prompt_str("Enter the output file: ", fn)
          
          DO WHILE (.NOT. (valid_ofile(fn, overwrite) .OR. quitter))
            IF (f_forbidden) THEN
              WRITE (*, 20) trim(fn)
              CALL prompt_str("Enter a different output file: ", fn)
            ELSE IF (f_exists) THEN 
              WRITE (*, 11) trim(fn)
              CALL prompt_str("Enter OVERWRITE to overwrite, QUIT " //
     &                        "to quit, or a different filename: ", fn)
              quitter = trim(fn) .EQ. "QUIT"
              overwrite = trim(fn) .EQ. "OVERWRITE"
            ELSE IF (f_isopen) THEN
              WRITE (*, 30) trim(fn)
              CALL prompt_str("Enter a different output file: ", fn)
            END IF
          END DO
        END SUBROUTINE
      END MODULE prompts

#define COL 1
#define ROW 2
#define MAT 3
      MODULE sum_matrices 
        USE IMPLICIT_SOME
        
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
        USE IMPLICIT_SOME
        USE sum_matrices
        USE files
        USE prompts

        IMPLICIT NONE
        CHARACTER(len=MAX_FILENAME) input
        LOGICAL :: quitter, overwrite
        INTEGER :: if_unit, of_unit

        input = '' 
        quitter = .FALSE.
        overwrite = .FALSE.

        CALL prompt_ifile(input, quitter)
        print *, input, quitter
        IF (quitter) STOP

        OPEN (file=input, newunit=if_unit, action='READ', 
     &  status='OLD', iostat=ios)

        DO WHILE (ios .EQ. 0)
        read (if_unit, '(A)', iostat=ios) input
        print *, TRIM(input)
        END DO
        print *, ios

        STOP
        
        CALL prompt_ofile(input, quitter)
        print *, input, quitter
        IF (quitter) STOP
        
        print *, "DONE!"
        
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
      STOP
      END PROGRAM

