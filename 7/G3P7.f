****************************************
* Program 7: Listed Links
* Group 3
*
*  Cody Baker 
*  Mikiya Dixon
*  Ravi Dressler
****************************************

* From Program 5
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
        SUBROUTINE ifileorbust (unit, bust)
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
              WRITE (*, '(A)') "! File does not exist."
              WRITE (*, '(A)', advance='no') "Try another file, quit "//
     &      "with 'QUIT', or type 'cookie' to feed the cookie monster: "
              READ (*, '(A)') fn
              bust = (trim(fn) .EQ. "QUIT")
              IF (.NOT. bust .AND. trim(fn) .EQ. "cookie")
     &          CALL cookiemonster ()
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

        SUBROUTINE ofileorbust (unit, bust)
          IMPLICIT NONE
          INTEGER, INTENT(OUT) :: unit
          LOGICAL, INTENT(OUT) :: bust
          LOGICAL :: nofile, exists

          bust = .FALSE.
          nofile = .TRUE.
          WRITE (*, '(A)', advance='no') "Enter output file: "
          READ (*, '(A)') fn
          DO WHILE (nofile .AND. .NOT. bust)

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

        SUBROUTINE cookiemonster ()
          IMPLICIT NONE
*          INTEGER, PARAMETER :: chocolatechips=9999999
          INTEGER, PARAMETER :: chocolatechips=99
          INTEGER, DIMENSION(:), POINTER :: cookie
          INTEGER :: i

          ALLOCATE (cookie(chocolatechips))
          cookie = [(i, i = 1, chocolatechips)]

          NULLIFY (cookie)
          WRITE (*, '(A)') "munch"
        END SUBROUTINE
      END MODULE

* Linked list and subroutines.
      MODULE roulette
        IMPLICIT NONE

        TYPE :: item_t
          CHARACTER(len=48) :: name
          INTEGER :: count2(9999)
          INTEGER :: count
        END TYPE

        TYPE :: node_t 
          TYPE (node_t), POINTER :: prv, nxt
          TYPE (item_t) :: item
        END TYPE

        TYPE :: list_t
          TYPE (node_t), POINTER :: head, tail
        END TYPE
        
      CONTAINS

* Set head = tail = NULL
      TYPE (list_t) FUNCTION make_list() 
        IMPLICIT NONE
        NULLIFY (make_list%head, make_list%tail)
      END FUNCTION

* Create and add a node from item
      SUBROUTINE list_append_item (list, item)
        IMPLICIT NONE
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (item_t), INTENT(IN) :: item
        TYPE (node_t), POINTER :: node
        INTEGER :: st

        ALLOCATE (node, stat=st)
        IF (st .NE. 0) 
     &    STOP "! Could not allocate space for a node, stopping."

        node%item = item
        CALL list_append (list, node)
      END SUBROUTINE

* Add an existing node (single, pointers cut)
      SUBROUTINE list_append (list, node)
        IMPLICIT NONE
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER, INTENT(INOUT) :: node

        NULLIFY (node%nxt, node%prv)
*   Empty list edge case
        IF (.NOT. associated(list%tail)) THEN 
          list%tail => node
          list%head => node
        ELSE
          node%prv => list%tail
          list%tail%nxt => node
          list%tail => node
        END IF
      END SUBROUTINE

* DO NOT call this like "node = walk (node, 2, over)"
* Auto deref will kill you. Use '=>'. 
      FUNCTION walk (from, steps, overstep)
        IMPLICIT NONE
        TYPE (node_t), POINTER, INTENT(IN) :: from
        INTEGER, INTENT(IN) :: steps
        TYPE (node_t), POINTER :: walk
        INTEGER, INTENT(OUT) :: overstep

*   Don't be stupid and pass in null.
        walk => from
        overstep = steps

        DO WHILE (overstep .GT. 0 .AND. associated(walk%nxt)) 
          walk => walk%nxt
          overstep = overstep - 1
          print *, "step"
        END DO

        DO WHILE (overstep .LT. 0 .AND. associated(walk%prv)) 
          walk => walk%prv
          overstep = overstep + 1
          print *, "step", walk%item%name
        END DO
      END FUNCTION

* Remove an existing node
      SUBROUTINE list_remove (list, node)
        IMPLICIT NONE
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER, INTENT(INOUT) :: node

*   If head, don't reach back, set head to next.
*   And/or if tail, don't reach ahead, set tail to prev.
*   (Prev/next should be null if list is singular).
        IF (associated(node, list%head)) THEN
          list%head => node%nxt
        ELSE
          node%prv%nxt => node%nxt
        END IF
        IF (associated(node, list%tail)) THEN
          list%tail => node%prv
        ELSE
          node%nxt%prv => node%prv
        END IF

*   Free the thing.
        DEALLOCATE (node)
        NULLIFY (node)
      END SUBROUTINE

* Deallocate every node in the list.
      SUBROUTINE list_annihilate (list)
        IMPLICIT NONE
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER :: oldhead
      
        DO WHILE (associated(list%head))
          oldhead => list%head
          list%head => oldhead%nxt
          DEALLOCATE(oldhead)
          CALL list_print (list)
        END DO

        NULLIFY (list%head, list%tail)
      END SUBROUTINE

* Print it
      SUBROUTINE list_print (list) 
        IMPLICIT NONE
        TYPE (list_t), INTENT(IN) :: list
        TYPE (node_t), POINTER :: node
        INTEGER :: i

        PRINT *, "--------------------"
        PRINT *, "HED: ",list%head%item%name,"TAL: ",list%tail%item%name

        node => list%head
        i = 0
        DO WHILE (associated(node))
          PRINT *, " NODE: ", i, TRIM(node%item%name)
          IF (associated(node%nxt)) THEN
            WRITE (*, '(A,A)', advance='no')
     &  " nxt: ", TRIM(node%nxt%item%name)
          ELSE
            WRITE (*, '(A)', advance='no') " nxt: null"
          END IF
          IF (associated(node%prv)) THEN
            WRITE (*, '(A,A)', advance='no')
     &  " prv: ", TRIM(node%prv%item%name)
          ELSE
            WRITE (*, '(A)', advance='no') " prv: null"
          END IF
          PRINT *, ''
          node => node%nxt
        END DO
        PRINT *, "--------------------"
      END SUBROUTINE

      END MODULE

* Read records into list
* Do the thing
* Print survivor
      PROGRAM main
        USE fileprompts
        USE roulette
        IMPLICIT NONE
        INTEGER, PARAMETER :: MAX_RECORDS=25

        TYPE (list_t) :: list
        INTEGER :: iunit, ounit, WAIT, i
        LOGICAL :: bust

        TYPE (node_t), POINTER :: at

        list = make_list()
        
        CALL ifileorbust (iunit, bust)
        CALL populate (list, iunit)
        CLOSE (iunit)
        
        at => list%head
        
        CALL list_print (list)
        WAIT = 0
        DO WHILE (WAIT .NE. 69)
          WRITE (*, *) "athead", associated(at, list%head), 
     & "attail", associated(at, list%tail), "name", at%item%name
          WRITE (*, *) "step to"
          READ (*, *) WAIT
          PRINT *, WAIT
          at => walk(at, WAIT, i)
          WRITE (*, *) "overstep", i
        CALL list_print (list)
        END DO
        CALL list_annihilate (list)

        READ (*, *) WAIT
        PRINT *, "Goodbye!"

      CONTAINS

* Read records into (end of) list from unit.
      SUBROUTINE populate (list, unit)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: unit
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (item_t) :: record
        INTEGER :: ios

        READ (unit, '(A)', iostat=ios) record%name
        READ (unit, '(I32)', iostat=ios) record%count
        DO WHILE (ios .EQ. 0)
          CALL list_append_item (list, record)
          READ (unit, '(A)', iostat=ios) record%name
          READ (unit, '(I32)', iostat=ios) record%count
        END DO
      END SUBROUTINE

      END PROGRAM
