      SUBROUTINE     read_qcube()

      implicit none

      include "parameter.h"
      include "variable.h"
      include "common.h"

      write(6,*) 'Please, type the name of your cube file.'
       read(*,*)        file_name

      open(i_unit, file=file_name, status='old')
C
C     ------------------------------------------------------------------
C     Reading ESP, for more info on the format go to http://gaussian.com/cubegen/

      read(i_unit,243)  title
      read(i_unit,243)  memo

      read(i_unit,244)    ATM, X0, Y0, Z0
      read(i_unit,244)     N1, X1, Y1, Z1
      read(i_unit,244)     N2, X2, Y2, Z2
      read(i_unit,244)     N3, X3, Y3, Z3

      do i = 1, ATM
      read(i_unit,245)  atom_num(i), a_num(i), XI(i), YI(i), ZI(i)
      enddo

      do 50 k = 1, N1
      do 60 l = 1, N2

      read(i_unit,246) (esp_QP(m,l,k), m = 1, N3)

   60 continue
   50 continue

C     ------------------------------------------------------------------

  243 format(A72)
  244 format(2X,I3,3F12.6)
  245 format(2X,I3,4F12.6)
  246 format(6E13.5)

      close(i_unit)

      END
