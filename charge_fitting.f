      PROGRAM     charge_fitting

C     i_CHARGE COPYRIGHTE © 2018 VERSION 5.0   
C     January 2016 - August 2018 by Oleg N. Starovoytov  

C     DEPARTMENT OF COMPUTER SCIENCE, 
C     SOUTHERN UNIVERSITY AND A&M COLLEGE,
C     BATON ROUGE, LA 70812
C
C     NORTHERN SOLUTIONS LLC, 
C     141 GRANITE DR.
C     WILLIAMSTON, MI 48895
C
C     Last updated: Thu Sep  8 15:55:40 EDT 2016 
C     Last updated: Fri May  7 22:46:46 EDT 2016 
C     Last updated: Thu Oct 11 23:00:00 EDT 2017 
C     Last updated: Sat Aug  4 17:06:55 JST 2018

C     This program is free software; you can redistribute it and/or
C     modify it under the terms of the GNU General Public License
C     as published by the Free Software Foundation; either version 2
C     of the License, or (at your option) any later version.
C
C     https://www.gnu.org/licenses/quick-guide-gplv3.html
C  
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.

C     THE VERSION 5.0 HAS BEEN DEVELOPED TO FIT
C     PARTIAL ATOMIC CHARGES FOR MOLECULAR DYNAMICS
C     SIMULATIONS FOR A VERIETY OF ORGANIC COMPOUNDS.
C     A GRID OF ELECTROSTATIC POTENTIAL POINTS IS
C     GENERATED FROM QM CALCULATIONS AND THESE 
C     POINTS ARE FITTED TO AN ATOM-CENTERED POINT 
C     CHARGE MODEL. PARTIAL ATOMIC CHARGES ARE THEN
C     USED TO CALCULATE MOLECULAR MULTIPOLE MOMENTS.
C
C     ELECTROSTATIC POTENTIAL POINTS THAT LAY WITHIN
C     THE VAN DER WAALS ATOMIC SURFACES ARE EXCLUDED 
C     FROM THE FITTING TO AVOID CONTRIBUTION FROM 
C     EXCHANGE REPULSION AND CHARGE TRANSFER INTERACTIONS. 
C     
C     A SQUARE MATRIX A IS BUILT AND THEN IT IS 
C     DECOMPOSED USING LU (A = L U) OR SVD (A = U W V) 
C     DECOMPOSITION TECHNIQUES. ONE OF THESE TWO TECHNIQUES
C     CAN BE CHOISEN BY PUTING .true. VALUE IN parameter.h FILE.  
C
C     CONSTRAINTS

C     Any constraints imposed should be based on
C     atom order number as it is listed in your 
C     cube file. The charge constrains should be 
C     imposed in a crossed closed way, see example below.
C
C     Example of an input file (charge.inp) for
C     dimethylcarbonate molecule.

C     # Number    Name  Constraint  Fix    Charge
C         1        O        0        0     0.000000 
C         2        C        2        0     0.000000
C         3        O        5        0     0.000000
C         4        C        6        0     0.000000
C         5        O        3        0     0.000000
C         6        C        4        0     0.000000 
C         7        H        8        0     0.000000
C         8        H        9        0     0.000000
C         9        H       10        0     0.000000
C        10        H       11        0     0.000000
C        11        H       12        0     0.000000
C        12        H        7        1     0.015000

C     CHARGE

C     0 should stand for if a charge is not equal to
C     any other charges by symmetry in the Constraint column.

C     0 should stand for if a charge is not fixed or 
C     predefined to any particular values in the Fix column.

C     1 should stand for if a charge is fixed or 
C     predefined to any particular values in the Fix column.

C     Predefined charges should be placed in the Charge column.

C     -----------------------------------------------------------------

      implicit none
 
      include "parameter.h"
      include "variable.h"
      include "common.h"

      i_unit   = 50
      j_unit   = 51
      k_unit   = 52
      l_unit   = 53
      m_unit   = 54
      n_unit   = 55
      o_unit   = 56
      p_unit   = 57
      q_unit   = 58

      write(6,*) '-------------------------------------------'
      write(6,*) 'Bohr radius: ', Bohr
      write(6,*) '-------------------------------------------'

               call     set_name()
               call   set_radius()

               call   read_qcube()
               call   make_input()
      if (CRT) call   read_input()

               call build_matrix()
      if (CRT) call const_matrix()

      if (LUD) call    solve_lud()
      if (SVD) call    solve_svd()

               call print_matrix()
               call print_charge()
      END

C     -----------------------------------------------------------------
