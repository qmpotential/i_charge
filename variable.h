      integer        i, j, k, l, m
      integer        i_unit, j_unit, k_unit, l_unit, m_unit
      integer        n_unit, o_unit, p_unit, q_unit
      integer        ATM, N1, N2, N3 
      integer        atom_num(NAP)
      integer        NAT, indx(NAP), jndx(NAP)
      integer        NOC, NOF, numi(NAP), numj(NAP), fix(NAP)

      real*8         X0, Y0, Z0, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3
      real*8         a_num(NAP), XI(NAP), YI(NAP), ZI(NAP)
      real*8         esp_QP(350,350,350)
      real*8         esp_FX(350,350,350)
      real*8         esp_FY(350,350,350)
      real*8         esp_FZ(350,350,350)
      real*8         exc_PT(350,350,350)
     
      real*8         vdw_radius(121), atom_mass(121)

      real*8         i_RX, i_RY, i_RZ, i_RX2, i_RY2, i_RZ2, i_RSD
      real*8         j_RX, j_RY, j_RZ, j_RX2, j_RY2, j_RZ2, j_RSD
      real*8         inv_RSD1, inv_RSD2

      real*8         C(NAP,NAP), F(NAP,NAP), q(NAP)

      real*8         bohr_vdw(NAP), A(NAP,NAP), B(NAP)
      real*8         D,DUM,XL(NAP,NAP),XU(NAP,NAP)
      real*8         U(NAP,NAP), V(NAP,NAP), W(NAP), X(NAP), Y(NAP)
      real*8         TOTAL_CHARGE

      character*3    atom_name(121)
      character*8    ext
      character*144  title, memo
      character*72   file_name

      character txt*3
