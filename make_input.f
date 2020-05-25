      SUBROUTINE     make_input()

      implicit none

      include "parameter.h"
      include "variable.h"
      include "common.h"

      logical lexist

      INQUIRE(file='charge.inp', EXIST=lexist) 

      if (lexist)       write(6,*) '----------------------------'
      if (lexist)       write(6,*) 'AN INPUT FILE HAS BEEN FOUND'
      if (lexist)       write(6,*) '----------------------------'

C     ------------------------------------------------------------------


      if (lexist) then

      open(p_unit,  file='charge.inp', status='unknown')

   30 read(p_unit,222,end=60) 

C      numi(i), atom_name(i)
      goto 30

   60 continue

      close(p_unit)


      endif

C     ------------------------------------------------------------------

      if (.not.lexist) then  

         write(6,*) '------------------------'
         write(6,*) 'AN INPUT FILE IS CREATED'
         write(6,*) '------------------------'

      open(p_unit,  file='charge.inp', status='new')

      write(p_unit,*)'#Number     Name     Constraint    Fix     Charge'

      do i = 1, ATM
      write(p_unit,223)  i, atom_name(atom_num(i)), 0, 0, 0.0d0
      enddo

      endif

C     ------------------------------------------------------------------

  221 format(A144)
  222 format(I8,5X,A3)
  223 format(I8,5X,A3,5X,I8,5X,I3,5X,F16.9)

      close(p_unit)

      END
