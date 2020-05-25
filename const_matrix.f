      SUBROUTINE     const_matrix()

C     ------------------------------------------------------------------
C     For more details, see the reference: O. N. Starovoytov and S. Yang 
C 
      include "parameter.h"
      include "variable.h"
      include "common.h"

C     ------------------------------------------------------------------
C                           Suffle A matrix according to the constraints
      i = 0

      do 220 j = 1, ATM

         if (numj(j).ne.0) then

             i = i + 1

             k = numi(j)
             l = numj(j)

             m = abs(l - k)

             if (m.eq.0) then 
 
                 numi(i + ATM) = k 
                 numj(i + ATM) = l 

             endif

             if (k.lt.l) then 

                 numi(i + ATM) = k 
                 numj(i + ATM) = l 

             endif

             if (k.gt.l) then

                 numi(i + ATM) = k 
                 numj(i + ATM) = l

             endif 

         endif
         
  220 enddo

C     ------------------------------------------------------------------
C                                           Writing down the constraints

      do 230 i = ATM + 1, ATM + NOC

             k = numi(i)
             l = numj(i)

         if (k == l) then

         A(k,i)  =  1.0d0
         A(i,k)  = -1.0d0

         endif

         if (k < l) then

         A(k,i)  =  1.0d0
         A(i,k)  =  1.0d0
         A(l,i)  = -1.0d0
         A(i,l)  = -1.0d0

         endif

         if (k > l) then

         A(k,i)  =  1.0d0
         A(i,k)  =  1.0d0
         A(l,i)  = -1.0d0
         A(i,l)  = -1.0d0

         endif

  230 enddo

C     ------------------------------------------------------------------
C                                      Fixed charges for matrix A.CONSTR
      i = 0

      do 240 j = 1, ATM

         if (fix(j)==1) then

             i = i + 1 

         A( j, i + ATM + NOC )  = 1.0d0
         A( i + ATM + NOC, j )  = 1.0d0

         endif

  240 enddo

C     ------------------------------------------------------------------
C                                      Fixed charges for matrix B.CONSTR
      i = 0

      do 260 j = 1, ATM

         if (fix(j)==1) then

             i = i + 1

             k = fix(j)

             fix( i + ATM + NOC ) = k 
               q( i + ATM + NOC ) = q(j)

         endif

  260 enddo   


      do 270 j = ATM + NOC + 1, ATM + NOC + NOF

         B( j )  = q(j)
      
  270 enddo   

C     ------------------------------------------------------------------
C                             Finilizing constraints for matrix A.CONSTR

      do 280 i = 1, ATM + NOC + NOF
      do 290 j = 1, ATM + NOC + NOF

      if ( i == (ATM + NOC + NOF) ) A(i + 1, j - NOC - NOF) = 1.0d0
      if ( j == (ATM + NOC + NOF) ) A(i - NOC - NOF, j + 1) = 1.0d0

      if ( i == (ATM + NOC + NOF) .and. j == (ATM + NOC + NOF) ) then

                                    A(i + 1, j + 1) = 0.0d0
      endif

  290 enddo
  280 enddo
  
      ATM = ATM + NOC + NOF

      END
