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

      MODULE nodes
        IMPLICIT NONE

        TYPE :: item_t
          CHARACTER(len=48) :: name
          INTEGER :: count
        END TYPE

        TYPE :: node_t 
          TYPE (node_t), POINTER :: prv, nxt
          TYPE (item_t) :: item
        END TYPE
        
      CONTAINS

* Allocate + initialize a node, return pointer to it.
      FUNCTION make_node (item)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: make_node
        TYPE (item_t), INTENT(IN) :: item
        INTEGER :: st

        ALLOCATE (make_node, stat=st)
        IF (st .NE. 0) STOP "! Failed to allocate a node, stopping."

        NULLIFY (make_node%nxt, make_node%prv)
        make_node%item = item
      END FUNCTION

* Link n1 <-> n2, handling nulls.
      SUBROUTINE node_link (n1, n2)
        IMPLICIT NONE
        TYPE (node_t), POINTER, INTENT(INOUT) :: n1, n2
        IF (associated(n1)) n1%nxt => n2
        IF (associated(n2)) n2%prv => n1
      END SUBROUTINE

* Add a node before / after another node, stitching together prv / nxt pointers.
      SUBROUTINE node_insert_nxt (node, nxt) 
        IMPLICIT NONE
        TYPE (node_t), POINTER, INTENT(INOUT) :: node, nxt
        CALL node_link(nxt, node%nxt)
        CALL node_link(node, nxt)
      END SUBROUTINE 

* ! Both insert_nxt & insert_prv will split lists if inserting a null.
      SUBROUTINE node_insert_prv (node, prv) 
        IMPLICIT NONE
        TYPE (node_t), POINTER, INTENT(INOUT) :: node, prv
        CALL node_link(node%prv, prv)
        CALL node_link(prv, node)
      END SUBROUTINE 

* Returns reference to the node at 'steps' steps away from origin node. 
* Any overflow (NULL reached before steps == 0) is returned in 'steps' itself.
      FUNCTION node_at (origin, steps)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: node_at
        TYPE (node_t), POINTER, INTENT(IN) :: origin
        INTEGER, INTENT(INOUT) :: steps

*   Don't be stupid and pass a null. Not handling that.
        node_at => origin
        DO WHILE (steps .GT. 0 .AND. associated(node_at%nxt)) 
          node_at => node_at%nxt
          steps = steps - 1
        END DO
        DO WHILE (steps .LT. 0 .AND. associated(node_at%prv)) 
          node_at => node_at%prv
          steps = steps + 1
        END DO
      END FUNCTION

* Link a node's prv and nxt without changing internal prv / nxt. Returns node.
      FUNCTION excise_node (node)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: excise_node
        TYPE (node_t), POINTER, INTENT(INOUT) :: node
        CALL node_link(node%prv, node%nxt)
        excise_node => node
      END FUNCTION

* Free from memory.
      SUBROUTINE release_node (node)
        IMPLICIT NONE
        TYPE (node_t), POINTER, INTENT(INOUT) :: node

        DEALLOCATE (node)
        NULLIFY (node)
      END SUBROUTINE
        
      END MODULE

* Linked list and subroutines.
      MODULE lists
        USE nodes
        IMPLICIT NONE

        TYPE :: list_t
          TYPE (node_t), POINTER :: head, tail
        END TYPE

      CONTAINS

      SUBROUTINE init_list (list) 
        IMPLICIT NONE
        TYPE (list_t), INTENT(OUT) :: list
        NULLIFY (list%head, list%tail)
      END SUBROUTINE

      SUBROUTINE list_append (list, node) 
        IMPLICIT NONE
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER, INTENT(INOUT) :: node
*   Empty list edge case.
        IF (.NOT. associated(list%head)) list%head => node
        CALL node_link (list%tail, node)
        list%tail => node
      END SUBROUTINE

* Same as node function + circularity.
      FUNCTION list_node_at (list, origin, steps)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: list_node_at
        TYPE (list_t), INTENT(IN) :: list
        TYPE (node_t), POINTER, INTENT(IN) :: origin
        INTEGER, INTENT(IN) :: steps
        INTEGER :: i

        i = steps
        list_node_at => node_at(origin, i)
        DO WHILE (i .GT. 0)
          i = i - 1
          list_node_at => node_at(list%head, i)
        END DO
        DO WHILE (i .LT. 0)
          i = i + 1
          list_node_at => node_at(list%tail, i)
        END DO
      END FUNCTION

* Same as node function + head/tail tracking.
      FUNCTION list_excise_node (list, node)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: list_excise_node
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER, INTENT(INOUT) :: node
        list_excise_node => excise_node(node)
        IF (associated(list_excise_node, list%head)) 
     &    list%head => list%head%nxt
        IF (associated(list_excise_node, list%tail)) 
     &    list%tail => list%tail%prv

      END FUNCTION

* Print it
      SUBROUTINE list_print (list) 
        IMPLICIT NONE
        TYPE (list_t), INTENT(IN) :: list
        TYPE (node_t), POINTER :: n
        CHARACTER(len=48) prvname, nxtname
        INTEGER :: i

        prvname = '!!!!'
        nxtname = '!!!!'
        IF (associated(list%head)) 
     &    WRITE (prvname,'(A)') list%head%item%name
        IF (associated(list%tail)) 
     &    WRITE (nxtname,'(A)') list%tail%item%name
        WRITE (*,'("===", "HEAD:", A, "===", "TAIL:", A, "===")')
     &    trim(prvname),trim(nxtname)

        i = 1
        n => list%head
        DO WHILE (associated(n))
          prvname = '!!!!'
          nxtname = '!!!!'
          IF (associated(n%prv)) WRITE (prvname,'(A)') n%prv%item%name
          IF (associated(n%nxt)) WRITE (nxtname,'(A)') n%nxt%item%name
          WRITE (*,'(I2, "|", "NAME:", A8, "|", "COUNT:",
     &               I2, "|", "PREV:", A8, "|", "NEXT:", A8, "|")')
     &    i,trim(n%item%name),n%item%count,trim(prvname),trim(nxtname)
          n => n%nxt
          i = i + 1
        END DO
      END SUBROUTINE

      END MODULE

* Read records into list
* Do the thing
* Print survivor
      PROGRAM main
        USE fileprompts
        USE lists
        USE nodes
        IMPLICIT NONE
        INTEGER, PARAMETER :: MAX_RECORDS=25

        TYPE (list_t) :: list
        INTEGER :: iunit, ounit
        LOGICAL :: bust

* Todo limit to MAX_RECORDS and check for zero records in input.

        CALL init_list (list)
        CALL list_print(list)
        
        CALL ifileorbust (iunit, bust)
        CALL populate (list, iunit)
        CLOSE (iunit)
        
        CALL list_print(list)
        CALL blamegame(list)
        CALL list_print(list)
        
        PRINT *, "Goodbye!"

      CONTAINS

* Read records into (end of) list from unit.
      SUBROUTINE populate (list, unit)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: unit
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER :: node
        TYPE (item_t) :: record
        INTEGER :: ios

        READ (unit, '(A)', iostat=ios) record%name
        READ (unit, '(I32)', iostat=ios) record%count
        DO WHILE (ios .EQ. 0)
          node => make_node(record)
          CALL list_append (list, node)
          READ (unit, '(A)', iostat=ios) record%name
          READ (unit, '(I32)', iostat=ios) record%count
        END DO
      END SUBROUTINE
      
      SUBROUTINE blamegame (list)
        IMPLICIT NONE
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER :: blamed, blamer 
        
        blamer => list%head
        blamed => list_node_at(list, blamer, blamer%item%count)
        WRITE (*, '(A," has ",I2," grievences, and accuses ",A)')
     &  trim(blamer%item%name),blamer%item%count,trim(blamed%item%name)

        DO WHILE (.NOT. associated(list%head, list%tail))
          blamer => list_excise_node(list, blamed)
          blamed => list_node_at(list, blamer, blamer%item%count)
        WRITE (*, '(A," has ",I2," excuses, and blames ",A)')
     &  trim(blamer%item%name),blamer%item%count,trim(blamed%item%name)
          CALL release_node (blamer)
        END DO

        blamer => list_excise_node(list, blamed)
        WRITE (*,
     &  '(A," has ",I2," excuses, but there is no one left to blame.")')
     &  trim(blamer%item%name), blamer%item%count
        CALL release_node (blamer)
      END SUBROUTINE


      END PROGRAM


