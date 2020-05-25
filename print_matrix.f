      SUBROUTINE    print_matrix()

C     ------------------------------------------------------------------
C     For more details, see the reference: B. H. Besler, K. M. Merz, Jr. 
C     and P. A. Kollman J. Comp. Chem. 1990, vol. 11, No. 4, 431-439
C 
      include "parameter.h"
      include "variable.h"
      include "common.h"

C     ------------------------------------------------------------------
C                                      For unconstrained matrix A.MATRIX
      if (.NOT.CRT) then 

      open(k_unit,  file='A.MATRIX', status='unknown')
      open(l_unit,  file='B.MATRIX', status='unknown')


      do 250 k = 1, ATM

         write(k_unit,248) (A(k,l),l = 1, ATM)
         write(l_unit,249)  Y(k)

  250 enddo
 
      endif

C     ------------------------------------------------------------------
C                                        For constrained matrix A.CONSTR

      if (CRT) then

      open(o_unit,  file='A.CONSTR', status='unknown')
      open(p_unit,  file='B.CONSTR', status='unknown')

      do 260 i = 1, ATM

         write(o_unit,248) ( A(i,j),j = 1, ATM )
         write(p_unit,249)  B(i)

  260 enddo

      endif

C     ------------------------------------------------------------------

      close(k_unit)
      close(l_unit)
      close(o_unit)
      close(p_unit)

  248 format(10000F18.9)
  249 format(F18.9)

      END
