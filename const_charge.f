      SUBROUTINE     const_charge()

      include "parameter.h"
      include "variable.h"
      include "common.h"
C
C     ------------------------------------------------------------------
      







C     ------------------------------------------------------------------
C                                               Writing matrix to a file
      do 230 i = 1, ATM + 1 

         write(j_unit,249) (A(i,j),j = 1, ATM + 1)
         write(k_unit,248)  B(i)

  230 enddo
  
C     ------------------------------------------------------------------


  248 format(F18.9)
  249 format(10000F18.9)

      END
