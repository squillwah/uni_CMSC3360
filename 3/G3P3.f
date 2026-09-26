

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

      SUBROUTINE read_matrices(matrices, count, cols, rows)
        IMPLICIT NONE
        integer :: i, c, r
        integer, intent(IN) :: cols, rows, count
        integer, intent(OUT) :: matrices(cols, rows, count)

*        matrices = reshape([(i, i = 1, 800)], [count, cols, rows])

        do i = 1, count
          do r = 1, rows
            print *, (matrices(c,r,i), c = 1, cols)
          END DO
        END DO
*        print *, ((matrices(1,c,r), c = 1, cols), r = 1, rows)
      
      RETURN
      END SUBROUTINE


      PROGRAM THREE
    
*        integer, parameter :: ASPECT = 3
*        integer, dimension(ASPECT) :: tensor_shape
*        real, dimension

        integer, parameter :: MAX_COL = 10, MAX_ROW = 10, MAX_MAT = 10
        integer :: matrices(MAX_COL*MAX_ROW*MAX_MAT), cols, rows, count

*        call mstuff (m, mshape)

        matrices = 0
        count = 8
        cols = 10
        rows = 10 

        matrices = [(i, i = 1, count*cols*rows)]

        print *, "test"

        print *, shape(matrices), size(matrices)

*        call mstuff(matrices, cols, rows)

        call read_matrices(matrices, count, rows, cols)

      STOP
      END PROGRAM


