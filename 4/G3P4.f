****************************************
* Program 4: Matrix Calculator
* Group 3
*
*  Cody Baker 
*  Mikiya Dixon
*  Ravi Dressler
****************************************

      PROGRAM main
        IMPLICIT NONE
        INTEGER, PARAMETER :: ROW=1, COL=2
* Enough space for three matrices, to store the sum/product/transposition. 
        INTEGER, PARAMETER :: MAX_ROWS=10, MAX_COLS=10, MAX_MATS=3

        TYPE :: MatrixBuffer
          INTEGER :: top = 1
          REAL, DIMENSION(MAX_ROWS*MAX_COLS*MAX_MATS) :: buff = 0
        END TYPE
* Matrix shape coupled with pointer into buffer.
        TYPE :: MatrixMeta
          INTEGER :: rxc(2) = 0
          REAL, DIMENSION(:,:), POINTER :: matrix => null()
        END TYPE

        TYPE (MatrixBuffer), TARGET :: matbuff
        TYPE (MatrixMeta) :: m1, m2, m3

        LOGICAL :: running
        INTEGER :: choice
        INTEGER :: ios

        running = .TRUE.

        DO WHILE (running)
          WRITE (*, '(A)') "What do you want to do? "
          WRITE (*, '(A)') " 1. Add two matrices"
          WRITE (*, '(A)') " 2. Subtract two matrices"
          WRITE (*, '(A)') " 3. Multiply two matrices"
          WRITE (*, '(A)') " 4. Transpose a matrix"
          WRITE (*, '(A)') " 5. Quit "
          WRITE (*, '(A)', advance='no') 
     &    "Press the number, then press enter: "

          READ (*, *, iostat=ios) choice
          DO WHILE (ios .NE. 0 .OR. choice .LT. 1 .OR. choice .GT. 5)
            WRITE (*, '(A)', advance='no') 
     & "Press 1, 2, 3, 4, or 5 on your keyboard. Then, press enter: "
            READ (*, *, iostat=ios) choice
          END DO

         SELECT CASE (choice)
         CASE (1)
          WRITE (*, '(A)') "Addition. You will now enter two matrices."
          WRITE (*, '(A)') "They must share their shape."
          CALL perform_addition ()
         CASE (2)
          WRITE (*, '(A)') "Subtraction. The second will be subtracted."
          WRITE (*, '(A)') "They both must share their shape."
          CALL perform_subtraction ()
         CASE (3)
          WRITE (*, '(A)') "Dot product multiplication. Rows * columns."
          WRITE (*, '(A)') "First's column # must equal second's row #."
          CALL perform_multiplication ()
         CASE (4)
          WRITE (*, '(A)') "Transposition. Swapping rows and columns."
          CALL perform_transposition ()
         CASE (5)
          WRITE (*, '(A)') "Goodbye!"
          running = .FALSE.
         END SELECT
        END DO

        CONTAINS 

        SUBROUTINE scaledsum (s1, s2)
          IMPLICIT NONE
          INTEGER, INTENT(IN) :: s1, s2
          
          CALL read_matrix_shape (m3)
          m1%rxc = m3%rxc
          m2%rxc = m3%rxc

          CALL matbuff_alloc (m1)
          CALL matbuff_alloc (m2)
          CALL matbuff_alloc (m3)
          
          WRITE (*, '(A)') "MATRIX ONE"
          CALL read_matrix_components (m1)
          CALL print_matrix (m1)
          WRITE (*, '(A)') "MATRIX TWO"
          CALL read_matrix_components (m2)
          CALL print_matrix (m2)

          WRITE (*, '(A)') "THE RESULT MATRIX"
          m3%matrix = (s1 * m1%matrix) + (s2 * m2%matrix)
          CALL print_matrix (m3)
          
          CALL matbuff_clear()
        END SUBROUTINE

        SUBROUTINE perform_addition ()
          IMPLICIT NONE
          CALL scaledsum (1, 1)
        END SUBROUTINE
        
        SUBROUTINE perform_subtraction ()
          IMPLICIT NONE
          CALL scaledsum (1, -1)
        END SUBROUTINE

        SUBROUTINE perform_multiplication ()
          IMPLICIT NONE
          INTEGER :: r, c
          WRITE (*, '(A)') "MATRIX ONE SHAPE"
          CALL read_matrix_shape (m1)
          WRITE (*, '(A)') "MATRIX TWO SHAPE"
          CALL read_matrix_shape (m2)
          DO WHILE (m1%rxc(COL) .NE. m2%rxc(ROW))
            WRITE (*, '(A)') "Columns must equal rows! Mxn * nxP"
            WRITE (*, '(A)') "MATRIX ONE SHAPE"
            CALL read_matrix_shape (m1)
            WRITE (*, '(A)') "MATRIX TWO SHAPE"
            CALL read_matrix_shape (m2)
          END DO

          m3%rxc = [m1%rxc(ROW), m2%rxc(COL)]

          CALL matbuff_alloc (m1)
          CALL matbuff_alloc (m2)
          CALL matbuff_alloc (m3)
          
          WRITE (*, '(A)') "MATRIX ONE"
          CALL read_matrix_components (m1)
          CALL print_matrix (m1)
          WRITE (*, '(A)') "MATRIX TWO"
          CALL read_matrix_components (m2)
          CALL print_matrix (m2)

          DO c = 1, m3%rxc(COL)
            DO r = 1, m3%rxc(ROW)
              m3%matrix(r,c) = DOT_PRODUCT(m1%matrix(r,:), 
     &                                     m2%matrix(:,c))
            END DO
          END DO
          
          WRITE (*, '(A)') "THE PRODUCT MATRIX"
          CALL print_matrix (m3)

          CALL matbuff_clear ()
        END SUBROUTINE

        SUBROUTINE perform_transposition ()
          IMPLICIT NONE
          WRITE (*, '(A)') "MATRIX SHAPE"
          CALL read_matrix_shape (m1)
          m2%rxc = [m1%rxc(COL), m1%rxc(ROW)]

          CALL matbuff_alloc (m1)
          CALL matbuff_alloc (m2)

          CALL read_matrix_components (m1)
          CALL print_matrix (m1)
         
          m2%matrix = TRANSPOSE(m1%matrix) 
          
          WRITE (*, '(A)') "THE TRANSPOSED MATRIX"
          CALL print_matrix (m2)

          CALL matbuff_clear ()
        END SUBROUTINE

        SUBROUTINE read_matrix_shape (m)
          IMPLICIT NONE
          TYPE (MatrixMeta), INTENT(INOUT) :: m
          WRITE (*, '(A)', advance='no') "Enter row and column count: "
          READ (*, *, iostat=ios) m%rxc
          DO WHILE (ios .NE. 0
     &         .OR. m%rxc(ROW) .GT. MAX_ROWS .OR. m%rxc(ROW) .LT. 1 
     &         .OR. m%rxc(COL) .GT. MAX_COLS .OR. m%rxc(COL) .LT. 1)
            WRITE (*, '(A, I2, A, I2, A)') 
     &   "Shape must be in range (1x1) to (", MAX_ROWS,'x',MAX_COLS,")."
            WRITE (*, '(A)', advance='no') "Try again: "
            READ (*, *, iostat=ios) m%rxc
          END DO
        END SUBROUTINE

        SUBROUTINE read_matrix_components (m)
          IMPLICIT NONE
          TYPE (MatrixMeta), INTENT(INOUT) :: m
          INTEGER :: r, c

* Read in row by row, so you can type it in as a square.
          WRITE (*, '(A,I2,X,I2,A)') 
     &    "Enter the ", m%rxc, " matrix below, in row major style:"
          READ (*, *, iostat=ios) ((m%matrix(r, c), c=1, m%rxc(COL)), 
     &                                              r=1, m%rxc(ROW))
          DO WHILE (ios .NE. 0)
            WRITE (*, '(A)') "Bad input. Try that again:"
            READ (*, *, iostat=ios) ((m%matrix(r, c), c=1, m%rxc(COL)),
     &                                                r=1, m%rxc(ROW))
          END DO
        END SUBROUTINE

        SUBROUTINE print_matrix (m)
          IMPLICIT NONE
          TYPE (MatrixMeta), INTENT(IN) :: m
          INTEGER :: r, c

          DO r = 1, m%rxc(ROW)
            WRITE (*, *) (m%matrix(r, c), c = 1, m%rxc(COL))
          END DO
        END SUBROUTINE

* Simple stack like thing. 
* No bothering with popping individual matrices.
* Just reset head index after each operation to "clear" memory.
        SUBROUTINE matbuff_alloc (m)
          IMPLICIT NONE
          TYPE (MatrixMeta), INTENT(INOUT) :: m
* To point a 2D pointer to a 1D array, one must perform 'rank remapping'.
* No copy is done, unlike RESHAPE. Part of 2008 standard. Not in the book. 
* https://fortranwiki.org/fortran/show/Fortran+2008+status
          m%matrix(1:m%rxc(ROW), 1:m%rxc(COL)) =>
     &    matbuff%buff(matbuff%top:)
          matbuff%top = matbuff%top + product(m%rxc)
        END SUBROUTINE

        SUBROUTINE matbuff_clear ()
          IMPLICIT NONE
          m1%rxc = 0
          m2%rxc = 0
          m3%rxc = 0
          NULLIFY (m1%matrix, m2%matrix, m3%matrix)
          
          matbuff%top = 1
        END SUBROUTINE

      END PROGRAM
