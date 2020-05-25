      subroutine solve_lu()
C     
C     -----------------------------------------------------------------
C          Lower triangular and Upper triangular (LU) decomposition 
C          subroutines, ludcmp.f and lubksb.f, are taken from Numerical 
C          Recipies in FORTRAN 77 Book by W. H. Press, S. A. Teukolsky,
C          W. T. Vetterling, B. P. Flannery, vol.1, pages 38 - 39 (1997).
C            
C          LU decomposition will solve a set of linear equations 
C          or a matrix if matrix is not singular. ( Ax=b ) 
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

      open(m_unit,  file='LU.matrix',  status='unknown')

C
C     -----------------------------------------------------------------
C       Performing Lower and Upper triangular decomposition of matrix A

      NAT = ATM + 1

      call ludcmp(A,NAT,NAP,indx,d)

        write(6,*)'----------------------------'
      do 90 i=1, NAT
        write(*,221) (A(i,j), j=1, NAT)
   90 continue

      do 20 k = 1, NAT
        do 30 l = 1, NAT

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

      do 40 k = 1, NAT
        write(m_unit,221) (XL(k,l), l = 1, NAT)
   40 continue

      write(m_unit,*) '#   Upper matrix of the decomposition:'

      do 50 k = 1, NAT
        write(m_unit,221) (XU(k,l), l = 1, NAT)
   50 enddo

C     -----------------------------------------------------------------

      call lubksb(A,NAT,NAP,indx,B)

      TOTAL_CHARGE = 0.0d0

      write(l_unit,*) '# Number     Name      Charge'

      do 23 k = 1, ATM
            write(l_unit,222) k,atom_name(atom_num(k)), B(k)
            TOTAL_CHARGE = TOTAL_CHARGE + B(k)
   23 enddo

      write(l_unit,*) 'Total molecular charge is ', TOTAL_CHARGE

  221 format(10000F12.6)
  222 format(I7,8X,A3,2X,1000F12.6)

      close (m_unit)

      END
