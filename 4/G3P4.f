



      PROGRAM main

        INTEGER, PARAMETER :: OP_ADD=0
        INTEGER, PARAMETER :: OP_SUB=1
        INTEGER, PARAMETER :: OP_DOT=2
        INTEGER, PARAMETER :: OP_TRANSPOSE=3
        
        TYPE :: Calculation
            INTEGER :: operation
            INTEGER :: cols, rows, mats
            REAL, POINTER, DIMENSION(:,:,:) matrices
        END TYPE Calculation



        IMPLICIT NONE

        INTEGER :: cols, rows, mats

        PRINT *, "Dimensions?"
        READ (*, *) cols, rows

        PRINT *, "How many matrices?"
        READ (*, *) mats

        PRINT *, "How many matrices?"
