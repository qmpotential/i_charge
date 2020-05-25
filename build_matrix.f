      SUBROUTINE     build_matrix()

C     ------------------------------------------------------------------
C     For more details, see the reference: B. H. Besler, K. M. Merz, Jr. 
C     and P. A. Kollman J. Comp. Chem. 1990, vol. 11, No. 4, 431-439
C 
      include "parameter.h"
      include "variable.h"
      include "common.h"


C     ------------------------------------------------------------------
C                                                  Initializing bohr_vdw
      do  5 i = 1, ATM

      bohr_vdw(i) = vdw_radius(atom_num(i)) / Bohr

    5 enddo

C     ------------------------------------------------------------------
C                                                  Initializing matrix A  
      do 10 i = 1, ATM
      do 20 j = 1, ATM

            A(i,j) = 0.0d0

   20 enddo
   10 enddo

C     ------------------------------------------------------------------
C                                                  Initializing matrix B
      do 30 i = 1, ATM

            B( i ) = 0.0d0

   30 enddo

C     ------------------------------------------------------------------
C                                          Initializing a grid of points 
      do 40 k = 1, N1
      do 50 l = 1, N2
      do 60 m = 1, N3

      esp_FX(m,l,k) = 0.0d0 
      esp_FY(m,l,k) = 0.0d0
      esp_FZ(m,l,k) = 0.0d0

      exc_PT(m,l,k) = 0.0d0

   60 enddo
   50 enddo
   40 enddo

C     ------------------------------------------------------------------
C                                              Building a grid of points 

      do 70 k = 1, N1
      do 80 l = 1, N2
      do 90 m = 1, N3

      esp_FX(m,l,k) = X0 + (k-1) * X1 + (l-1) * X2 + (m-1) * X3
      esp_FY(m,l,k) = Y0 + (k-1) * Y1 + (l-1) * Y2 + (m-1) * Y3
      esp_FZ(m,l,k) = Z0 + (k-1) * Z1 + (l-1) * Z2 + (m-1) * Z3

   90 enddo
   80 enddo
   70 enddo

C     ------------------------------------------------------------------
C                              Exclude points that lay within vdw radius

      do 100 i = 1, ATM

      do 110 k = 1, N1
      do 120 l = 1, N2
      do 130 m = 1, N3

C     ------------------------------------------------------------------
C          Calculating distances to ESP points from atom i to each point

      i_RX = XI(i) - esp_FX(m,l,k)
      i_RY = YI(i) - esp_FY(m,l,k)
      i_RZ = ZI(i) - esp_FZ(m,l,k)

      i_RX2 = i_RX * i_RX
      i_RY2 = i_RY * i_RY
      i_RZ2 = i_RZ * i_RZ

      i_RSD = SQRT( i_RX2 + i_RY2 + i_RZ2 )

C     ------------------------------------------------------------------
C                                 Building a list of excluded ESP points

      if (i_RSD.lt.bohr_vdw(i)) exc_PT(m,l,k) = esp_QP(m,l,k) 

  130 enddo
  120 enddo
  110 enddo

  100 enddo

C     ------------------------------------------------------------------
C                                                   Calculating matrix A
      do 140 i = 1, ATM
      do 150 j = 1, ATM

      do 160 k = 1, N1
      do 170 l = 1, N2
      do 180 m = 1, N3

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
C                    Calculating distances from atom j to each ESP point

      j_RX = XI(j) - esp_FX(m,l,k)
      j_RY = YI(j) - esp_FY(m,l,k)
      j_RZ = ZI(j) - esp_FZ(m,l,k)

      j_RX2 = j_RX * j_RX
      j_RY2 = j_RY * j_RY
      j_RZ2 = j_RZ * j_RZ

      j_RSD = SQRT( j_RX2 + j_RY2 + j_RZ2 )

C     ------------------------------------------------------------------
C                                                 Building matrix A(i,j)

      if (i_RSD.ge.bohr_vdw(i).and.j_RSD.ge.bohr_vdw(j)) then 

          if ( esp_QP(m,l,k).ne.exc_PT(m,l,k) ) then

          inv_RSD2 = 1.0d0 / ( i_RSD * j_RSD )

          A(i,j) =  A(i,j) + inv_RSD2

          endif

      endif 

  180 enddo
  170 enddo
  160 enddo

  150 enddo
  140 enddo

C     ------------------------------------------------------------------
C                                                  Calculating  matrix B
      do 200 k = 1, N1
      do 210 l = 1, N2
      do 220 m = 1, N3

C     ------------------------------------------------------------------
C                    Calculating distances from atom i to each ESP point

      do 190 i = 1, ATM

      i_RX = XI(i) - esp_FX(m,l,k)
      i_RY = YI(i) - esp_FY(m,l,k)
      i_RZ = ZI(i) - esp_FZ(m,l,k)

      i_RX2 = i_RX * i_RX
      i_RY2 = i_RY * i_RY
      i_RZ2 = i_RZ * i_RZ

      i_RSD = SQRT( i_RX2 + i_RY2 + i_RZ2 )

C     ------------------------------------------------------------------
C                                                   Building matrix B(i)
      if ( i_RSD.ge.bohr_vdw(i) ) then

          if ( esp_QP(m,l,k).ne.exc_PT(m,l,k) ) then

          inv_RSD1 = 1.0d0 / ( i_RSD )

          B( i ) = B( i ) + ( esp_QP(m,l,k) * inv_RSD1 )
          Y( i ) = B( i )

          endif

      endif

  190 enddo

  220 enddo
  210 enddo
  200 enddo

C     ------------------------------------------------------------------
C                             Finilizing constraints for matrix A.CONSTR

      if (.NOT.CRT) then

      do 230 i = 1, ATM
      do 240 j = 1, ATM

      if ( i == (ATM)) A(i+1, j) = 1.0d0
      if ( j == (ATM)) A(i, j+1) = 1.0d0

      if ( i == (ATM) .and. j == (ATM) ) A(i + 1, j + 1) = 0.0d0

  240 enddo
  230 enddo

      ATM = ATM + 1 

      endif

      END
