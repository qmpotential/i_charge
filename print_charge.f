      SUBROUTINE    print_charge()

C     ------------------------------------------------------------------
C                                               Printing partial charges  
      include "parameter.h"
      include "variable.h"
      include "common.h"

C     ------------------------------------------------------------------

      open(q_unit,  file='CHARGE.OUT',   status='unknown')

      TOTAL_CHARGE = 0.0d0

      do 5 i = 1, ATM
     
         if (LUD) q(i) = B(i)
         if (SVD) q(i) = X(i)

    5 enddo

C     ------------------------------------------------------------------

      write(q_unit,*)'#Number     Name     Constraint    Fix     Charge'

      if (CRT) then

      do 10 j = 1, ATM - NOC - NOF - 1
            write(q_unit,222)  numi(j), atom_name(atom_num(j)), 
     &                         numj(j),                 fix(j),     q(j)
                 TOTAL_CHARGE = TOTAL_CHARGE + q(j)
   10 enddo

      else

      do 15 j = 1, ATM
            write(q_unit,222)  j, atom_name(atom_num(j)),
     &                   numj(j),                 fix(j),      q(j)
                 TOTAL_CHARGE = TOTAL_CHARGE + q(j)
   15 enddo

      endif

      write(q_unit,*) '----------------------------------------'
      write(q_unit,*) 'Total molecular charge is ', TOTAL_CHARGE

C     ------------------------------------------------------------------

  222 format(I8,5X,A3,5X,I8,5X,I3,5X,F16.9)

      close(q_unit) 

      END
