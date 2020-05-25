      SUBROUTINE     set_radius()

      implicit none

      include "parameter.h"
      include "variable.h"
      include "common.h"

C     -----------------------------------------------------------------
C     Van der Waals radius (Rw) can be described as a size of a single 
C     ioslated atom. Van der Waals radii are taken from several sources: 
C     1) Bondi, A. J. Phys. Chem. 1964, 68, 3, 441-451;
C     2) Batsanov, S. S. Inorg. Mat., 37, 9, 2001, 871-885;
C     3) Pauling, L. The Nature of the Chemical Bond, Ithaca: Cornell
C     University, 1960, 3rd ed. All values are given in Angstroms (Å).
C     4)Breneman C. M. and Kenneth B. W. J. Comp. Chem 1990, 11, 3,
C     361-373.

      vdw_radius(0)   =  1.2d0          ! Lone pair (LP)

      vdw_radius(1)   =  1.2d0          ! H  (Breneman C. M. and Kenneth B. W. 1990 1.45)
      vdw_radius(2)   =  1.4d0          ! He
      vdw_radius(3)   =  1.82d0         ! Li
      vdw_radius(4)   =  2.23d0         ! Be (Batsanov S. S., 2001)
      vdw_radius(5)   =  2.05d0         ! B  (Batsanov S. S., 2001)
      vdw_radius(6)   =  1.5d0          ! C  (Breneman C. M. and Kenneth B. W. 1990 1.5) 
      vdw_radius(7)   =  1.70d0         ! N  (Breneman C. M. and Kenneth B. W. 1990)
      vdw_radius(8)   =  1.40d0         ! O  (Breneman C. M. and Kenneth B. W. 1990 1.7)
      vdw_radius(9)   =  1.47d0         ! F
      vdw_radius(10)  =  1.54d0         ! Ne
      vdw_radius(11)  =  2.27d0         ! Na
      vdw_radius(12)  =  1.73d0         ! Mg
      vdw_radius(13)  =  2.40d0         ! Al (Batsanov S. S., 2001)
      vdw_radius(14)  =  2.10d0         ! Si
      vdw_radius(15)  =  1.80d0         ! P
      vdw_radius(16)  =  1.75d0         ! S (1.80)
      vdw_radius(17)  =  1.75d0         ! Cl
      vdw_radius(18)  =  1.88d0         ! Ar
      vdw_radius(19)  =  2.75d0         ! K 
      vdw_radius(20)  =  2.78d0         ! Ca (Batsanov S. S., 2001)

      vdw_radius(21)  =  2.62d0         ! Sc (Batsanov S. S., 2001) 
      vdw_radius(22)  =  2.44d0         ! Ti (Batsanov S. S., 2001)
      vdw_radius(23)  =  2.27d0         ! V  (Batsanov S. S., 2001)
      vdw_radius(24)  =  2.23d0         ! Cr (Batsanov S. S., 2001)
      vdw_radius(25)  =  2.25d0         ! Mn (Batsanov S. S., 2001)
      vdw_radius(26)  =  2.27d0         ! Fe (Batsanov S. S., 2001)
      vdw_radius(27)  =  2.25d0         ! Co (Batsanov S. S., 2001)
      vdw_radius(28)  =  1.63d0         ! Ni
      vdw_radius(29)  =  1.40d0         ! Cu 
      vdw_radius(30)  =  1.39d0         ! Zn
      vdw_radius(31)  =  1.87d0         ! Ga
      vdw_radius(32)  =  2.32d0         ! Ge (Batsanov S. S., 2001)
      vdw_radius(33)  =  1.85d0         ! As
      vdw_radius(34)  =  1.90d0         ! Se
      vdw_radius(35)  =  1.85d0         ! Br
      vdw_radius(36)  =  2.02d0         ! Kr
      vdw_radius(37)  =  3.15d0         ! Rb (Batsanov S. S., 2001)
      vdw_radius(38)  =  2.94d0         ! Sr (Batsanov S. S., 2001)
      vdw_radius(39)  =  2.71d0         ! Y  (Batsanov S. S., 2001)
      vdw_radius(40)  =  2.57d0         ! Zr (Batsanov S. S., 2001)

      vdw_radius(41)  =  2.46d0         ! Nb (Batsanov S. S., 2001)
      vdw_radius(42)  =  2.39d0         ! Mo (Batsanov S. S., 2001) 
      vdw_radius(43)  =  2.37d0         ! Tc (Batsanov S. S., 2001)
      vdw_radius(44)  =  2.37d0         ! Ru (Batsanov S. S., 2001)
      vdw_radius(45)  =  2.32d0         ! Rh (Batsanov S. S., 2001)
      vdw_radius(46)  =  1.63d0         ! Pd
      vdw_radius(47)  =  1.72d0         ! Ag
      vdw_radius(48)  =  1.58d0         ! Cd 
      vdw_radius(49)  =  1.93d0         ! In
      vdw_radius(50)  =  2.17d0         ! Sn
      vdw_radius(51)  =  2.41d0         ! Sb (Batsanov S. S., 2001)
      vdw_radius(52)  =  2.06d0         ! Te
      vdw_radius(53)  =  1.98d0         ! I
      vdw_radius(54)  =  2.16d0         ! Xe
      vdw_radius(55)  =  3.30d0         ! Cs (Batsanov S. S., 2001)
      vdw_radius(56)  =  3.05d0         ! Ba (Batsanov S. S., 2001)
      vdw_radius(57)  =  2.81d0         ! La (Batsanov S. S., 2001)
      vdw_radius(58)  =  0.0d0          ! Ce
      vdw_radius(59)  =  0.0d0          ! Pr
      vdw_radius(60)  =  0.0d0          ! Nd

      vdw_radius(61)  =  0.0d0          ! Pm
      vdw_radius(62)  =  0.0d0          ! Sm
      vdw_radius(63)  =  0.0d0          ! Eu
      vdw_radius(64)  =  0.0d0          ! Gd
      vdw_radius(65)  =  0.0d0          ! Tb
      vdw_radius(66)  =  0.0d0          ! Dy
      vdw_radius(67)  =  0.0d0          ! Ho
      vdw_radius(68)  =  0.0d0          ! Er
      vdw_radius(69)  =  0.0d0          ! Tm
      vdw_radius(70)  =  0.0d0          ! Yb
      vdw_radius(71)  =  0.0d0          ! Lu
      vdw_radius(72)  =  2.52d0         ! Hf (Batsanov S. S., 2001)
      vdw_radius(73)  =  2.42d0         ! Ta (Batsanov S. S., 2001)
      vdw_radius(74)  =  2.36d0         ! W  (Batsanov S. S., 2001)
      vdw_radius(75)  =  2.35d0         ! Re (Batsanov S. S., 2001)
      vdw_radius(76)  =  2.33d0         ! Os (Batsanov S. S., 2001)
      vdw_radius(77)  =  2.34d0         ! Ir (Batsanov S. S., 2001)
      vdw_radius(78)  =  1.75d0         ! Pt
      vdw_radius(79)  =  1.66d0         ! Au
      vdw_radius(80)  =  1.55d0         ! Hg

      vdw_radius(81)  =  1.96d0         ! Tl
      vdw_radius(82)  =  2.02d0         ! Pb
      vdw_radius(83)  =  3.52d0         ! Bi (Batsanov S. S., 2001)
      vdw_radius(84)  =  0.0d0          ! Po
      vdw_radius(85)  =  0.0d0          ! At
      vdw_radius(86)  =  0.0d0          ! Rn
      vdw_radius(87)  =  0.0d0          ! Fr
      vdw_radius(88)  =  0.0d0          ! Ra
      vdw_radius(89)  =  0.0d0          ! Ac
      vdw_radius(90)  =  2.75d0         ! Th (Batsanov S. S., 2001)
      vdw_radius(91)  =  0.0d0          ! Pa
      vdw_radius(92)  =  1.86d0         ! U
      vdw_radius(93)  =  0.0d0          ! Np
      vdw_radius(94)  =  0.0d0          ! Pu
      vdw_radius(95)  =  0.0d0          ! Am
      vdw_radius(96)  =  0.0d0          ! Cm
      vdw_radius(97)  =  0.0d0          ! Bk
      vdw_radius(98)  =  0.0d0          ! Cf
      vdw_radius(99)  =  0.0d0          ! Es
      vdw_radius(100) =  0.0d0          ! Fm

      vdw_radius(101) =  0.0d0          ! Md
      vdw_radius(102) =  0.0d0          ! No
      vdw_radius(103) =  0.0d0          ! Lr
      vdw_radius(104) =  0.0d0          ! Rf
      vdw_radius(105) =  0.0d0          ! Db
      vdw_radius(106) =  0.0d0          ! Sg
      vdw_radius(107) =  0.0d0          ! Bh
      vdw_radius(108) =  0.0d0          ! Hs
      vdw_radius(109) =  0.0d0          ! Mt
      vdw_radius(110) =  0.0d0          ! Ds
      vdw_radius(111) =  0.0d0          ! Rg
      vdw_radius(112) =  0.0d0          ! Hs
      vdw_radius(113) =  0.0d0          ! Ds
      vdw_radius(114) =  0.0d0          ! Uub
      vdw_radius(115) =  0.0d0          ! Uut
      vdw_radius(116) =  0.0d0          ! Uuq
      vdw_radius(117) =  0.0d0          ! Uup
      vdw_radius(118) =  0.0d0          ! Uuh
      vdw_radius(119) =  0.0d0          ! Uus
      vdw_radius(120) =  0.0d0          ! Uuo

      END
