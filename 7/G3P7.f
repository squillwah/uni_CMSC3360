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

      FUNCTION make_node (item)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: make_node
        TYPE (item_t), INTENT(IN) :: item
        INTEGER :: memstat

        ALLOCATE (make_node, stat=memstat)
        IF (st .NE. 0) STOP "! Failed to allocate a node, stopping."

        NULLIFY (make_node%nxt, make_node%prv)
        make_node%item = item
      END FUNCTION

* Link n1 <-> n2, handling nulls.
      SUBROUTINE node_link (n1, n2)
        IMPLICIT NONE
        TYPE (node_t), POINTER, INTENT(INOUT) :: n1, n2
        IF (allocated(n1)) n1%nxt => n2
        IF (allocated(n2)) n2%prv => n1
      END SUBROUTINE

* Both insert_nxt & insert_prv will split lists if inserting a null!
      SUBROUTINE node_insert_nxt (node, nxt) 
        IMPLICIT NONE
        TYPE (node_t), POINTER, INTENT(INOUT) :: node, nxt
        node_link(nxt, node%nxt)
        node_link(node, nxt)
      END SUBROUTINE 

      SUBROUTINE node_insert_prv (node, prv) 
        IMPLICIT NONE
        TYPE (node_t), POINTER, INTENT(INOUT) :: node, prv
        node_link(node%prv, prv)
        node_link(prv, node)
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
        DO WHILE (steps .GT. 0 .AND. associated(walk%nxt)) 
          node_at => node_at%nxt
          steps = steps - 1
        END DO
        DO WHILE (steps .LT. 0 .AND. associated(walk%prv)) 
          node_at => node_at%prv
          steps = steps + 1
        END DO
      END FUNCTION

* Link a node's prv and nxt without changing internal prv / nxt. Returns node.
      FUNCTION excise_node (node)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: excise_node
        TYPE (node_t), POINTER, INTENT(INOUT) :: node
        node_link(node%prv, node%nxt)
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
        CALL node_link (list%tail, node)
        list%tail => node
      END SUBROUTINE

* Same as node function + circularity.
      FUNCTION list_node_at (list, origin, steps)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: list_node_at
        TYPE (list_t). INTENT (IN) :: list
        TYPE (node_t), POINTER, INTENT(IN) :: origin
        INTEGER, INTENT(IN) :: steps
        INTEGER :: i

        i = steps
        list_node_at => node_at(origin, i)
        DO WHILE (i .GT. 0)
          list_node_at => node_at(list%head, i-1)
        END DO
        DO WHILE (i .LT. 0)
          list_node_at => node_at(list%tail, i-1)
        END DO
      END FUNCTION

* Same as node function + head/tail tracking.
      FUNCTION list_excise_node (list, node)
        IMPLICIT NONE
        TYPE (node_t), POINTER :: list_excise_node
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER, INTENT(INOUT) :: node
        list_excise_node => excise_node(list, node)
        IF (associated(list_excise_node, list%head)) 
     &    list%head => list%head%nxt
        IF (associated(list_excise_node, list%tail)) 
     &    list%tail => list%head%prv
      END FUNCTION

      END MODULE

      SUBROUTINE blamegame (list)
        IMPLICIT NONE
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER, INTENT(IN) :: blamed, blamer 

C  *   Head stays first time around, special case.
C          blamer => list%head
C          blamed => list_node_at(list, blamer, blamer%count)
C  *   The blamed becomes the blamer, and blames some other guy. 
C          DO WHILE (.NOT. associated(list%head, list%tail))
C            blamer => list_excise_node(list, blamed)
C            blamed => list_node_at(list, blamer, blamer%count)
C          END DO
C  
C  *   Who did it? It was ...
C          blamer => list%head
C          DO WHILE (.NOT. associated(list%head, list%tail))
C  *   ... him!
C            blamed => list_node_at(list, blamer, blamer%count)
C  *   No! I'm innocent! It was ...
C            blamer => list_excise_node(list, blamed)
C          END DO

*   'Who did it? It was him!'
        blamer => list%head
        blamed => list_node_at(list, blamer, blamer%count)
        DO WHILE (.NOT. associated(list%head, list%tail))
*   'No! I'm innocent! It was him!'
          blamer => list_excise_node(list, blamed)
          blamed => list_node_at(list, blamer, blamer%count)
          CALL release_node (blamer)
        END DO
      END SUBROUTINE


C       * Set head = tail = NULL
C             TYPE (list_t) FUNCTION make_list() 
C               IMPLICIT NONE
C               NULLIFY (make_list%head, make_list%tail)
C             END FUNCTION
C       
C       * Create and add a node from item
C             SUBROUTINE list_append_item (list, item)
C               IMPLICIT NONE
C               TYPE (list_t), INTENT(INOUT) :: list
C               TYPE (item_t), INTENT(IN) :: item
C               TYPE (node_t), POINTER :: node
C               INTEGER :: st
C       
C               ALLOCATE (node, stat=st)
C               IF (st .NE. 0) 
C            &    STOP "! Could not allocate space for a node, stopping."
C       
C               node%item = item
C               CALL list_append_node (list, node)
C             END SUBROUTINE
C       
C       * Add an existing node (single, pointers cut)
C             SUBROUTINE list_append_node (list, node)
C               IMPLICIT NONE
C               TYPE (list_t), INTENT(INOUT) :: list
C               TYPE (node_t), POINTER, INTENT(INOUT) :: node
C       
C               NULLIFY (node%nxt, node%prv)
C       *   Empty list edge case
C               IF (.NOT. associated(list%tail)) THEN 
C                 list%tail => node
C                 list%head => node
C               ELSE
C                 node%prv => list%tail
C                 list%tail%nxt => node
C                 list%tail => node
C               END IF
C             END SUBROUTINE
C       
C       * Remove an existing node
C             SUBROUTINE list_cut_node (list, node)
C               IMPLICIT NONE
C               TYPE (list_t), INTENT(INOUT) :: list
C               TYPE (node_t), POINTER, INTENT(INOUT) :: node
C       
C       *   If head, don't reach back, set head to next.
C       *   And/or if tail, don't reach ahead, set tail to prev.
C       *   (Prev/next should be null if list is singular).
C               IF (associated(node, list%head)) THEN
C                 list%head => node%nxt
C               ELSE
C                 node%prv%nxt => node%nxt
C               END IF
C               IF (associated(node, list%tail)) THEN
C                 list%tail => node%prv
C               ELSE
C                 node%nxt%prv => node%prv
C               END IF
C             END SUBROUTINE
C       
C             SUBROUTINE free_node (node)
C               IMPLICIT NONE
C               TYPE (node_t), POINTER, INTENT(INOUT) :: node
C       
C               DEALLOCATE (node)
C               NULLIFY (node)
C             END SUBROUTINE
C       
C             SUBROUTINE list_delete_node (list, node)
C               IMPLICIT NONE
C               TYPE (list_t), INTENT(INOUT) :: list
C               TYPE (node_t), POINTER, INTENT(INOUT) :: node
C               CALL list_cut_node (list, node)
C               CALL free_node (node)
C             END SUBROUTINE
C       
C       * Deallocate every node in the list.
C             SUBROUTINE list_annihilate (list)
C               IMPLICIT NONE
C               TYPE (list_t), INTENT(INOUT) :: list
C               TYPE (node_t), POINTER :: oldhead
C             
C               DO WHILE (associated(list%head))
C                 oldhead => list%head
C                 list%head => oldhead%nxt
C                 DEALLOCATE(oldhead)
C               END DO
C       
C               NULLIFY (list%head, list%tail)
C             END SUBROUTINE
C       
C       
C       * Walk up or down from 'from' node.
C       * Sign of 'steps' is direction, overflow returned param.
C       * DO NOT call this with '=', auto deref will kill you, use '=>'. 
C             FUNCTION walk (from, steps, overstep)
C               IMPLICIT NONE
C               INTEGER, INTENT(IN) :: steps
C               INTEGER, INTENT(OUT) :: overstep
C               TYPE (node_t), POINTER :: walk
C               TYPE (node_t), POINTER, INTENT(IN) :: from
C       
C       *   Don't be stupid and pass a null. Not handling that.
C               walk => from
C               overstep = steps
C               DO WHILE (overstep .GT. 0 .AND. associated(walk%nxt)) 
C                 walk => walk%nxt
C                 overstep = overstep - 1
C       *          print *, "step", walk%item%name
C               END DO
C               DO WHILE (overstep .LT. 0 .AND. associated(walk%prv)) 
C                 walk => walk%prv
C                 overstep = overstep + 1
C       *          print *, "step", walk%item%name
C               END DO
C             END FUNCTION
C       
C       * Just like 'walk()' but loop around the given list.
C             FUNCTION walk_around (list, from, steps)
C               IMPLICIT NONE
C               TYPE (list_t), INTENT(IN) :: list
C               TYPE (node_t), POINTER :: walk_around
C               TYPE (node_t), POINTER, INTENT(IN) :: from
C               INTEGER, INTENT(IN) :: steps
C               INTEGER :: overstep
C       
C               walk_around => walk(from, steps, overstep)
C               DO WHILE (overstep .GT. 0)
C                 walk_around => walk(list%head, overstep-1, overstep)
C               END DO
C               DO WHILE (overstep .LT. 0)
C                 walk_around => walk(list%tail, overstep+1, overstep)
C               END DO
C             END FUNCTION
C       
C       * Print it
C             SUBROUTINE list_print (list) 
C               IMPLICIT NONE
C               TYPE (list_t), INTENT(IN) :: list
C               TYPE (node_t), POINTER :: node
C               INTEGER :: i
C       
C               PRINT *, "===================="
C               PRINT *, "HED: ",list%head%item%name,"TAL: ",list%tail%item%name
C               PRINT *, "--------------------"
C       
C               i = 0
C               node => list%head
C               DO WHILE (associated(node))
C                 PRINT *, " NODE: ", i, TRIM(node%item%name), node%item%count
C                 IF (associated(node%nxt)) THEN
C                   WRITE (*,'(A,A)', advance='no')
C            &  " nxt: ", TRIM(node%nxt%item%name)
C                 ELSE
C                   WRITE (*,'(A)', advance='no') " nxt: null"
C                 END IF
C                 IF (associated(node%prv)) THEN
C                   WRITE (*,'(A,A)', advance='no')
C            &  " prv: ", TRIM(node%prv%item%name)
C                 ELSE
C                   WRITE (*,'(A)', advance='no') " prv: null"
C                 END IF
C                 PRINT *, ''
C                 node => node%nxt
C               END DO
C               PRINT *, "===================="
C             END SUBROUTINE
C       
C             END MODULE

* Read records into list
* Do the thing
* Print survivor
      PROGRAM main
        USE fileprompts
        USE linkedlists
        IMPLICIT NONE
        INTEGER, PARAMETER :: MAX_RECORDS=25

        TYPE (list_t) :: list
        INTEGER :: iunit, ounit, WAIT, i
        LOGICAL :: bust

        TYPE (node_t), POINTER :: at, nxt, killer, victim, this, that

        list = make_list()
        
        CALL ifileorbust (iunit, bust)
        CALL populate (list, iunit)
        CLOSE (iunit)
        
        at => list%head
        
*        CALL list_print (list)
*        WAIT = 0
*        DO WHILE (WAIT .NE. 69)
*          WRITE (*, *) "athead", associated(at, list%head), 
*     & "attail", associated(at, list%tail), "name", at%item%name
*          WRITE (*, *) "step to"
*          READ (*, *) WAIT
*          PRINT *, WAIT
*          at => walk(at, WAIT, i)
*          WRITE (*, *) "overstep", i
*          CALL list_print (list)
*        END DO

*        CALL list_print (list)
*        victim => list%head
*        steps = list%head%item%count
*        DO WHILE (.NOT. associated(list%head, list%tail))
**          print *, "K: ", trim(killer%item%name), killer%item%count
*          victim =>
*          victim => walk_around(list, victim, steps)
*          this => walk_around(list, that, that%item%count)
*          that => walk_around(list, this, this%item%count)
*          victim => walk_around(list, killer, killer%item%count)
*          killer => walk_around(list, victim, victim%item%count)
**          that => walk_around(list, this, sign(1,victim%item%count))
**          print *, "V: ", trim(victim%item%name), victim%item%count
*          CALL list_remove (list, this)
*          CALL list_print (list)
*        END DO

*          print *, trim(at2%item%name), at2%item%count

        CALL list_print (list)
        CALL blamegame (list)
        CALL list_print (list)
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

      SUBROUTINE blamegame (list)
        TYPE (list_t), INTENT(INOUT) :: list
        TYPE (node_t), POINTER :: blamed, blamer
        INTEGER :: excuses

        blamer => list%head
        excuses = blamer%item%count
        blamed => walk_around(list, blamer, excuses)
        WRITE (*, *) "It was ", blamed%item%name
        
        blamer => blamed
        WRITE (*, *) blamer%item%name, "I didn't do it!"
        blamed => walk_around(list, blamed, sign(1,excuses))
        excuses = excuses - 1
        CALL list_delete_node (list, blamer)
        blamed => walk_around(list, blamed, sign(1,excuses))
        blamed = walk_around (list, 
        excuses = blamer%item%count
        WRITE (*, *) blamer%item%name, " did nothing wrong"
        blamed => walk_around(list, blamed, sign(1,excuses))
        excuses = excuses - 1
        CALL list_remove (list, blamer)
        blamed => walk_around(list, blamer, excuses)
        WRITE (*, *) blamed%item%name, " was at fault"
        
        blamed
*        blamer => list%head
                
        DO WHILE (.NOT. associated(list%head, list%tail))
          WRITE (*, *) blamer%item%name, " did nothing wrong"
          blamed => walk_around(list, blamer, excuses)
          WRITE (*, *) blamed%item%name, " was at fault"
          excuses = blamed%item%count
          blamer => walk_around(list, blamed, sign(1,excuses))
          CALL list_delete_node (list, blamed)
        END DO
      END SUBROUTINE



      END PROGRAM


*        direct = -(sign(1,steps))
*        DO WHILE (i .NE. 0)
*          list_node_at => node_at(origin, i)
*        END DO
