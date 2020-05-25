      subroutine solve_lud()
C     
C     -----------------------------------------------------------------
C          Lower triangular and Upper triangular (LU) decomposition 
C          subroutines, ludcmp.f and lubksb.f, are taken from Numerical 
C          Recipies in FORTRAN 77 Book by W. H. Press, S. A. Teukolsky,
C          W. T. Vetterling, B. P. Flannery, vol.1, pages 38 - 39 (1997).
C            
C          LU decomposition will solve a set of linear equations 
C          or a matrix if matrix is not singular. ( A x = b ) 
C          A singular matrix (a square matrix) is the matrix that can 
C          not be inversed. The matrix is singular if its determinant 
C          is ZERO. 
C   
C          Lower triangular and Upper triangular (LU) decomposition 
C          will fail to solve the singular matrix. SVD decomposition
C          will solve such a matrix, giving you a close approximation.

      implicit none

      include "parameter.h"
      include "variable.h"
      include "common.h"

      open(m_unit,  file='LUD.MATRIX',  status='unknown')
C
C     -----------------------------------------------------------------
C       Performing Lower and Upper triangular decomposition of matrix A


      call ludcmp(A,ATM,NAP,indx,d)

      do 20 k = 1, ATM
        do 30 l = 1, ATM

          if ( l.gt.k ) then
            XU(k,l) = A(k,l)
            XL(k,l) = 0.0d0

          else if ( l.lt.k ) then
            XU(k,l) = 0.0d0
            XL(k,l) = A(k,l)

          else
            XU(k,l) = A(k,l)
            XL(k,l) = 1.0d0
          endif

   30   continue
  20   continue

C     -----------------------------------------------------------------
C       Printing lower matrix and upper matrix after decomposition of A

      write(m_unit,*) '#   Lower matrix of the decomposition:'

      do 40 k = 1, ATM
        write(m_unit,221) (XL(k,l), l = 1, ATM)
   40 enddo

      write(m_unit,*) '#   Upper matrix of the decomposition:'

      do 50 k = 1, ATM
        write(m_unit,221) (XU(k,l), l = 1, ATM)
   50 enddo

C     -----------------------------------------------------------------

      call lubksb(A,ATM,NAP,indx,B)

  221 format(10000F18.9)

      close(m_unit)

      END
