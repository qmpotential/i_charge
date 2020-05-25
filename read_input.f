      SUBROUTINE     read_input()

      implicit none

      include "parameter.h"
      include "variable.h"
      include "common.h"

      open(j_unit,  file='charge.inp', status='unknown')

C     ------------------------------------------------------------------
C                              Reading input with all charge constrains.

      NOC = 0
      NOF = 0

       read(j_unit,221)    memo
           write(6,221)    memo  

      do 50 i = 1, ATM

      read(j_unit,222,end=60)  numi(i), atom_name(atom_num(i)), 
     &                         numj(i),                 fix(i),     q(i)

                 write(6,222)  numi(i), atom_name(atom_num(i)), 
     &                         numj(i),                 fix(i),     q(i)

      if (   numj(i).ne.0 ) NOC = NOC + 1
      if (    fix(i).ne.0 ) NOF = NOF + 1

   50 enddo
   60 continue

C     ------------------------------------------------------------------

  221 format(A144)
  222 format(I8,5X,A3,5X,I8,5X,I5,5X,F16.9)

      close (j_unit)

      END
