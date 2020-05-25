      subroutine solve_svd()
C     
C     -----------------------------------------------------------------
C          Singular value decomposition subroutines svbksb.f, pathog.f,
C          and svdcmp.f are taken from Numerical Recipies in FORTRAN 77 
C          Book by W. H. Press, S. A. Teukolsky, W. T. Vetterling, B. P. 
C          Flannery, vol.1, pages 53 - 59 (1997).
C            
C          Singular Value Decomposition (SVD) can be used to solve a 
C          set of linear equations or matices that are close to be
C          singular or singular. A singular matrix (a square matrix)
C          is the matrix that can not be inversed. The matrix is 
C          singular if its determinant is ZERO. 
C   
C          Lower triangular and Upper triangular (LU) decomposition 
C          will fail to solve the singular matrix. SVD decomposition
C          will solve such a matrix, giving you a close approximation.
C        
      implicit none

      include "parameter.h"
      include "variable.h"
      include "common.h"

      open(n_unit,  file='SVD.MATRIX', status='unknown')
C
C     -----------------------------------------------------------------
C                   Performing Singular Value Decomposition of matrix A

      ATM = ATM + 1

      do 12 k=1,ATM
        do 11 l=1,ATM
          U(k,l)=A(k,l)
11      continue
12    continue

      call svdcmp(U,ATM,ATM,NAP,NAP,W,V)

      write(n_unit,*) 'Decomposition Matrices:'
      write(n_unit,*) 'Matrix U'

      do 13 k=1,ATM
        write(n_unit,221) (U(k,l),l=1,ATM)
13    continue

      write(n_unit,*) 'Diagonal of Matrix W'
      write(n_unit,221) (W(k),k=1,ATM)
      write(n_unit,*) 'Matrix V-Transpose'

      do 14 k=1,ATM
        write(n_unit,'(1x,6f12.6)') (V(l,k),l=1,ATM)
14    continue

      write(n_unit,*) 'Check product against original matrix:'
      write(n_unit,*) 'Original Matrix:'

      do 15 k=1,ATM
        write(n_unit,221) (A(k,l),l=1,ATM)
15    continue

      write(n_unit,*) 'Product U*W*(V-Transpose):'

      do 18 k=1,ATM
        do 17 l=1,ATM
          A(k,l)=0.0
          do 16 j=1,ATM
            A(k,l)=A(k,l)+U(k,j)*W(j)*V(l,j)
16        continue
17      continue

        write(n_unit,221) (A(k,l),l=1,ATM)
18    continue

      call svbksb(U,W,V,ATM,ATM,NAP,NAP,B,X)

  221 format(10000F18.9)

      close (n_unit)

      END
