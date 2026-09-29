
* Shorthand for shape array access.
#define COLS 1
#define ROWS 2
#define MATS 3

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
        INTEGER i, c, r
        INTEGER ios
        LOGICAL tf
        CHARACTER(len=256) iom
      END MODULE

      MODULE files
        IMPLICIT NONE 

        INTEGER, PARAMETER :: MAX_FILENAME=255
        CHARACTER(len=2), PARAMETER :: FORBIDDEN_FILES(2) = ('.', '..')
      
      CONTAINS 

* Subroutines flag file existence in 'tf' & store io errors in ios / iom.
        SUBROUTINE prompt_infile (unit, prompt)
          IMPLICIT NONE 
          
          INTEGER, INTENT(OUT) :: unit
          CHARACTER(len=*), INTENT(IN) :: prompt

          CHARACTER(len=MAX_FILENAME) filename

          filename = ''

100       FORMAT (' ', T1, A)
200       FORMAT (A)
          WRITE (*, 100, advance='no') prompt
          READ (*, 200, iostat=ios, iomsg=iom) filename

* check that name is not . or ..

          
      END MODULE

      MODULE matrices 
        IMPLICIT NONE

        INTEGER, PARAMETER :: MAX_COLS=10, MAX_ROWS=10, MAX_MATS=10
        
* Flat memory space for matrices. 
* Dimensionality unknown until runtime. Read into 'aspect'.
* Subroutines interpret 'matrices' tensor shape using 'aspect'.
        REAL :: matrices(MAX_COLS*MAX_ROWS*MAX_MATS)
        REAL :: sum_matrix(MAX_COLS*MAX_ROWS)
        INTEGER :: aspect(3)
        
        CONTAINS

        SUBROUTINE prompt_fopen (prompt, unit, asout)
          IMPLICIT NONE

          CHARACTER(len=*), INTENT(IN) :: prompt
          LOGICAL, INTENT(IN) :: asout
          INTEGER, INTENT(OUT) :: unit

1000      FORMAT (' ', A)
1001      FORMAT (A)
          WRITE (*, 1000, advance='no') prompt
          READ (*, 1001, iostat=ios) filename

          INQUIRE (file=filename, exist=tf)

          PRINT *, tf
          
          RETURN
        END SUBROUTINE
      END MODULE file_io


      RECURSIVE SUBROUTINE prompt_filename (filename, prompt)

1000    FORMAT (' ', A)
1001    FORMAT (A)
        WRITE (*, 1000, advance='no') prompt
        READ (*, 1001, iostat=ios) filename

        IF (ios .NE. 0) THEN
          print *, "ass"
          CALL prompt_filename (filename, "Something bad happened.")
        END IF
*       ELSE IF <check file existence or smthn>
      RETURN
      END SUBROUTINE

      SUBROUTINE read_matrices (matrices, aspect)
        IMPLICIT NONE
        USE IMPLICIT_SOME
        USE matrices
        USE files
        
        INTEGER, INTENT(IN) :: aspect(3)
        REAL, INTENT(OUT) :: matrices(aspect(COLS), aspect(ROWS), aspect(MATS))


        DO i = 1, aspect(MATS) 
          DO r = 1, aspect(ROWS)
            PRINT *, (matrices(c,r,i), c = 1, aspect(COLS))
          END DO
        END DO

*        matrices = reshape([(i, i = 1, 800)], [count, cols, rows])
*        print *, ((matrices(1,c,r), c = 1, cols), r = 1, rows)
      RETURN
      END SUBROUTINE

      PROGRAM main
        USE file_io
        IMPLICIT NONE
*        INTEGER :: i
    
*        integer, parameter :: ASPECT = 3
*        integer, dimension(ASPECT) :: tensor_shape
*        real, dimension

*        INTEGER, PARAMETER :: MAX_FILENAME=255
        INTEGER, PARAMETER :: MAX_COLS=10, MAX_ROWS=10, MAX_MATS=10


        character(len=MAX_FILENAME) infile, outfile

        INTEGER :: ifunit
       
        matrices = 0
        aspect(COLS) = 10
        aspect(ROWS) = 10
        aspect(MATS) = 10

        infile = ''
        outfile = ''
        
        PRINT *, "test"
        PRINT *, shape(matrices), size(matrices)
        PRINT *, shape(sum_matrix), size(sum_matrix)
        PRINT *, aspect
        print *, MAX_COLS, MAX_ROWS, MAX_MATS

        matrices = [(i, i = 1, product(aspect))]

        CALL read_matrices(matrices, aspect)

        CALL prompt_filename(infile, "THE NAME?! ")

        print *, TRIM(infile), TRIM(outfile)

        CALL prompt_fopen ("test exists: ", ifunit, .FALSE.)

*        call mstuff (m, mshape)
*        call mstuff(matrices, cols, rows)
      STOP
      END PROGRAM




*      SUBROUTINE 
*
*
*      SUBROUTINE tensor (x, y, z, T)
*        IMPLICIT NONE
*
*        integer, intent(in) :: x, y, z
*        real, dimension(x,y,z), intent(out) :: T
*
*      RETURN
*      END SUBROUTINE

      SUBROUTINE mstuff (m, cols, rows)
        integer, intent(IN) :: cols, rows
        integer, intent(INOUT) :: m(cols, rows)

        print *, shape(m), size(m)

        m(11, 10) = 1

      RETURN
      END SUBROUTINE

