      subroutine solve_lsq()

      implicit none

      include "parameter.h"
      include "variable.h"
      include "common.h"

      open(o_unit,  file='LSQ.matrix',  status='unknown')

C     ------------------------------------------------------------------
C                                                  Initializing bohr_vdw
      do  5 i = 1, ATM

      bohr_vdw(i) = vdw_radius(atom_num(i)) / Bohr

    5 enddo

C     ------------------------------------------------------------------

      do 10 j = 1, ATM

         Ax(j) = 0.0d0 
         Bx(j) = 0.0d0 

   10 enddo

C     ------------------------------------------------------------------
C                                                  Calculating  matrix B

      do 200 k = 1, N1
      do 210 l = 1, N2
      do 220 m = 1, N3

      do 190 i = 1, ATM

C     ------------------------------------------------------------------
C                    Calculating distances from atom i to each ESP point

      i_RX = XI(i) - esp_FX(m,l,k)
      i_RY = YI(i) - esp_FY(m,l,k)
      i_RZ = ZI(i) - esp_FZ(m,l,k)

      i_RX2 = i_RX * i_RX
      i_RY2 = i_RY * i_RY
      i_RZ2 = i_RZ * i_RZ

      i_RSD = SQRT( i_RX2 + i_RY2 + i_RZ2 )

C     ------------------------------------------------------------------
C                                      Building matrices Ax(i) and Bx(i)
      if ( i_RSD.ge.bohr_vdw(i) ) then

          if ( esp_QP(m,l,k).ne.exc_PT(m,l,k) ) then

          inv_RSD1 = 1.0d0 / ( i_RSD )

C          esp_FF(m,l,k) = esp_FF(m,l,k) + q(i) * inv_RSD1

          endif

      endif

  190 enddo

  220 enddo
  210 enddo
  200 enddo


      do 230 i = 1, ATM

         write(o_unit,*) Ax(i), Bx(i)

 230  enddo

C     ------------------------------------------------------------------

      close (o_unit)

      END

