!Copyright (c) 2023, Kristopher G. Klein and Daniel Verscharen
!All rights reserved.
!
!This source code is licensed under the BSD-style license found in the
!LICENSE file in the root directory of this source tree.
!
!===============================================================================
!I                                                                             I
!I                              A  L  P  S                                     I
!I                     Arbitrary Linear Plasma Solver                          I
!I                                                                             I
!I                              Version 1.0                                    I
!I                                                                             I
!I  Kristopher Klein   (kgklein@arizona.edu)                                   I
!I  Daniel Verscharen  (d.verscharen@ucl.ac.uk)                                I
!I                                                                             I
!===============================================================================

module alps_distribution_analyt
  !! This module evaluates a pre-defined function for f0. This is an alternative
  !! to the use of an f0 table or a bi-Maxwellian approximation.
  !! This function can be used to define f0 for the integration or for the analytic
  !! continuation (or both).
  implicit none

  ! This interface block tells the module about the external BESSK function.
  ! It describes its arguments and what it returns.
  interface
    function BESSK_da(N,X)
      implicit none
      integer, intent(in) :: N
      double precision, intent(in) :: X
      double precision :: BESSK_da
    end function BESSK_da
  end interface

  public :: distribution_analyt

contains

! double complex function distribution_analyt(is,pperp,ppar,ms_s,beta_s,tau_s,alph_s)
double complex function distribution_analyt(is,pperp,ppar)
!! This function returns the pre-defined function as f0.
  implicit none

  integer, intent(in) :: is
  !! Index of species.

  double precision :: pperp
  !! Perpendicular momentum.

  double complex :: ppar
  !! Parallel momentum.

  double complex :: f0
  !! Value of the distribution function.

  double precision :: norm
  !! Normalisation factor for f0.

  double precision :: pi
  !! Pi.

  ! double precision, intent(in) :: ms_s

  ! double precision, intent(in) :: beta_s

  ! double precision, intent(in) :: tau_s

  ! double precision, intent(in) :: alph_s

  double precision :: BESSK, vA

  double precision :: beta
  !! Plasma beta.

  double precision :: ms
  !! Mass of species.

  !! --- DEFINE PARAMETERS LOCALLY ---

  !! Parameters for Protons (Species 1)
  double precision, parameter :: beta_p = 1.0d0
  double precision, parameter :: ms_p = 1.0d0
  double precision, parameter :: tau_p = 1.0d0
  double precision, parameter :: alph_p = 1.0d0
  ! double precision, parameter :: alph_p = 3.0d0 ! Anisotropy T_perp/T_para = 3

  !! Parameters for Electrons (Species 2)
  double precision, parameter :: beta_e = 1.0d0
  ! double precision, parameter :: ms_e = 5.44662d-4
  double precision, parameter :: ms_e = 1.0d0
  double precision, parameter :: tau_e = 1.0d0
  double precision, parameter :: alph_e = 1.0d0 ! Isotropic electrons`

  vA = 1.0d0

  pi=atan(1.d0)*4.d0


  ! This function defines the f0 table for the creation of distributions and for the analytic continuation.
  ! Ensure that the distribution is normalised. generate_distribution can do this automatically, but the fit
  ! routine of ALPS will not do this.
  ! Define the function for however many species you would like to do this.

  ! Remember that ppar is a complex variable. For most functions, this should not make a difference.
  ! In the following, define the complex f0 function of pperp and ppar for any number of species as needed.
  ! Only those species that are set to have either distributionS=0 in the generate_distribution input file or
  ! those species that have ff=0 in the ALPS input file will be treated in this way.
  ! Any other cases for species will be ignored.



  !select case(is)
  !
  !  case(1) ! Species 1
  !
  !    ! The example below illustrates how to set up a Maxwellian with beta = 1:
  !    beta=1.d0
  !    ms=1.d0
  !
  !    f0=(pi**(-1.5d0) /((ms * beta )**(3.d0/2.d0) )) * exp( -( ppar**2/( beta * ms)&
  !      + (pperp**2)/( beta * ms ) ) )
  !
  !  case(2) ! Species 2
  !
  !    ! The example below illustrates how to set up a Maxwellian with beta = 1:
  !    beta=1.d0
  !    ms=1.d0/1836.d0
  !
  !    f0=(pi**(-1.5d0) /((ms * beta )**(3.d0/2.d0) )) * exp( -( ppar**2/( beta * ms)&
  !      + (pperp**2)/( beta * ms ) ) )
  !
  !  end select

  ! select case(is)
  !
  !   case(1) ! Species 1
  !
  !     norm = pi**(-1.5d0) /((ms_s * beta_s * tau_s)**(3.d0/2.d0) * alph_s )
  !     f0 = exp( -( (ppar**2.d0) / ( beta_s * ms_s * tau_s) &
  !                 + (pperp**2.d0) / ( tau_s * beta_s * ms_s * alph_s ) ) )
  !     f0 = f0 * norm
  !
  !   case(2) ! Species 2
  !
  !     norm = pi**(-1.5d0) /((ms_s * beta_s * tau_s)**(3.d0/2.d0) * alph_s )
  !     f0 = exp( -( (ppar**2.d0) / ( beta_s * ms_s * tau_s) &
  !                 + (pperp**2.d0) / ( tau_s * beta_s * ms_s * alph_s ) ) )
  !     f0 = f0 * norm
  !
  ! end select

  ! select case(is)
  !   case(1) ! Protons
  !     norm = pi**(-1.5d0) / ((ms_p * beta_p * tau_p)**(1.5d0) * alph_p)
  !     f0 = exp( -( (ppar**2) / (beta_p * ms_p * tau_p) &
  !                + (pperp**2) / (tau_p * beta_p * ms_p * alph_p) ) )
  !     f0 = f0 * norm
  !
  !   case(2) ! Electrons
  !     norm = pi**(-1.5d0) / ((ms_e * beta_e * tau_e)**(1.5d0) * alph_e)
  !     f0 = exp( -( (ppar**2) / (beta_e * ms_e * tau_e) &
  !                + (pperp**2) / (tau_e * beta_e * ms_e * alph_e) ) )
  !     f0 = f0 * norm
  ! end select

  select case(is)
    case(1) ! Protons
      norm = vA/(2.d0*pi*sqrt(alph_p)*ms_p**2 * beta_p * tau_p )
      norm = norm / BESSK_da(2, 2.d0*ms_p/(vA*vA*alph_p*beta_p*tau_p))
      
      f0 = exp( -(2.d0*ms_p/(vA*vA*beta_p*tau_p*alph_p)) * &
            sqrt(1.d0 + pperp**2*vA**2/(ms_p**2) + &
              real(ppar)**2*vA**2*alph_p/(ms_p**2) ) )
      f0 = f0 * norm

    case(2) ! Electrons
      norm = vA/(2.d0*pi*sqrt(alph_e)*ms_e**2 * beta_e * tau_e )
      norm = norm / BESSK_da(2, 2.d0*ms_e/(vA*vA*alph_e*beta_e*tau_e))

      f0 = exp( -(2.d0*ms_e/(vA*vA*beta_e*tau_e*alph_e)) * &
            sqrt(1.d0 + pperp**2*vA**2/(ms_e**2) + &
              real(ppar)**2*vA**2*alph_e/(ms_e**2) ) )
      f0 = f0 * norm
    end select

distribution_analyt=f0

end function

end module

! --- External Bessel Functions ---
! These functions are now OUTSIDE the module block. They are compiled
! as independent, external functions that the linker can easily find.

  FUNCTION BESSK_da(N,X)
    implicit none
    integer, intent(in) :: N
    double precision, intent(in) :: X
    INTEGER J
    REAL *8 BESSK_da, BESSK0_da, BESSK1_da, TOX, BK, BKM, BKP
    IF (N.EQ.0) THEN
      BESSK_da = BESSK0_da(X)
      RETURN
    ENDIF
    IF (N.EQ.1) THEN
      BESSK_da = BESSK1_da(X)
      RETURN
    ENDIF
    IF (X.EQ.0.D0) THEN
      BESSK_da = 1.D30
      RETURN
    ENDIF
    TOX = 2.D0/X
    BK  = BESSK1_da(X)
    BKM = BESSK0_da(X)
    DO 11 J=1,N-1
      BKP = BKM+DFLOAT(J)*TOX*BK
      BKM = BK
      BK  = BKP
11  CONTINUE
    BESSK_da = BK
  END FUNCTION BESSK_da

  FUNCTION BESSK0_da(X)
    implicit none
    double precision, intent(in) :: X
    REAL*8 BESSK0_da,Y,AX,P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,BESSI0_da
    DATA P1,P2,P3,P4,P5,P6,P7/-0.57721566D0,0.42278420D0,0.23069756D0, &
          0.3488590D-1,0.262698D-2,0.10750D-3,0.74D-5/
    DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,-0.7832358D-1,0.2189568D-1, &
          -0.1062446D-1,0.587872D-2,-0.251540D-2,0.53208D-3/
    IF(X.EQ.0.D0) THEN
      BESSK0_da=1.D30
      RETURN
    ENDIF
    IF(X.LE.2.D0) THEN
      Y=X*X/4.D0
      AX=-LOG(X/2.D0)*BESSI0_da(X)
      BESSK0_da=AX+(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
    ELSE
      Y=(2.D0/X)
      AX=EXP(-X)/DSQRT(X)
      BESSK0_da=AX*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
    ENDIF
  END FUNCTION BESSK0_da

  FUNCTION BESSK1_da(X)
    implicit none
    double precision, intent(in) :: X
    REAL*8 BESSK1_da,Y,AX,P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,BESSI1_da
    DATA P1,P2,P3,P4,P5,P6,P7/1.D0,0.15443144D0,-0.67278579D0, &
          -0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
    DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,-0.3655620D-1, &
          0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3/
    IF(X.EQ.0.D0) THEN
      BESSK1_da=1.D32
      RETURN
    ENDIF
    IF(X.LE.2.D0) THEN
      Y=X*X/4.D0
      AX=LOG(X/2.D0)*BESSI1_da(X)
      BESSK1_da=AX+(1.D0/X)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
    ELSE
      Y=(2.D0/X)
      AX=EXP(-X)/DSQRT(X)
      BESSK1_da=AX*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
    ENDIF
  END FUNCTION BESSK1_da

  FUNCTION BESSI0_da(X)
    implicit none
    double precision, intent(in) :: X
    REAL *8 BESSI0_da,Y,P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,AX,BX
    DATA P1,P2,P3,P4,P5,P6,P7/1.D0,3.5156229D0,3.0899424D0,1.2067429D0, &
          0.2659732D0,0.360768D-1,0.45813D-2/
    DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,0.1328592D-1, &
          0.225319D-2,-0.157565D-2,0.916281D-2,-0.2057706D-1, &
          0.2635537D-1,-0.1647633D-1,0.392377D-2/
    IF(ABS(X).LT.3.75D0) THEN
      Y=(X/3.75D0)**2
      BESSI0_da=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
    ELSE
      AX=ABS(X)
      Y=3.75D0/AX
      BX=EXP(AX)/DSQRT(AX)
      AX=Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9)))))))
      BESSI0_da=AX*BX
    ENDIF
  END FUNCTION BESSI0_da

  FUNCTION BESSI1_da(X)
    implicit none
    double precision, intent(in) :: X
    REAL *8 BESSI1_da,Y,P1,P2,P3,P4,P5,P6,P7,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,AX,BX
    DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0, &
          0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
    DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1, &
          -0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1, &
          -0.2895312D-1,0.1787654D-1,-0.420059D-2/
    IF(ABS(X).LT.3.75D0) THEN
      Y=(X/3.75D0)**2
      BESSI1_da=X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
    ELSE
      AX=ABS(X)
      Y=3.75D0/AX
      BX=EXP(AX)/DSQRT(AX)
      AX=Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9)))))))
      BESSI1_da=AX*BX
    ENDIF
  END FUNCTION BESSI1_da
