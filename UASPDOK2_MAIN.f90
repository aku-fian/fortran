PROGRAM TEST_ALGORTMA_2
  IMPLICIT NONE
  
  REAL :: R
  ! Deklarasi fungsi SAL73 bertipe REAL
  REAL :: SAL73
  
  ! NILAI UJI DARI DOKUMEN UNESCO UNTUK ALGORITMA 2
  REAL, PARAMETER :: S_IN = 35.0      ! Salinitas Input
  REAL, PARAMETER :: T    = 15.0      ! Suhu (Derajat Celcius)
  REAL, PARAMETER :: P    = 1000.0    ! Tekanan (Decibar)
  INTEGER, PARAMETER :: M = 1         ! M=1 untuk Konversi Salinitas ke Rasio Konduktivitas
  
  ! Panggil fungsi SAL73
  R = SAL73(S_IN, T, P, M)
  
  ! Tampilkan hasil
  WRITE(*,*) 'Algoritma 2: Salinitas ke Rasio Konduktivitas'
  WRITE(*,*) 'Input Salinitas (S_IN): ', S_IN
  WRITE(*,*) 'Input T (Suhu): ', T
  WRITE(*,*) 'Input P (Tekanan): ', P
  WRITE(*,*) '--------------------------------------'
  WRITE(*,*) 'Rasio Konduktivitas Hasil Perhitungan (R): ', R
  WRITE(*,*) 'Nilai Target dari Dokumen: 0.9599661' 
  
END PROGRAM TEST_ALGORTMA_2


! ***************************************************************
! FUNGSI UTAMA SAL73 (ALGORITMA 2)
! ***************************************************************
REAL FUNCTION SAL73(CND, T, P, M)
  IMPLICIT NONE
  
  ! Deklarasi Argumen
  REAL, INTENT(IN) :: CND, T, P
  INTEGER, INTENT(IN) :: M
  
  ! Deklarasi fungsi pembantu
  REAL, EXTERNAL :: S_FROM_RT
  SUBROUTINE GET_DS_DRT(XR_in, T_in, DS_DRT_out)
  
  ! Variabel Lokal
  REAL :: DT, PBAR, RT35_VAL, E, RT, XR, S_CALC, S_TARGET
  REAL :: A_VAL, B_VAL, CP_VAL, DEN
  REAL :: XT, DELTA_S, DS_DRT_VAL
  INTEGER :: N

  ! ** Konstanta Koefisien ** (Hanya untuk Koreksi Tekanan)
  REAL, PARAMETER :: C1=2.070E-5, C2=-6.370E-10, C3=3.989E-15

  SAL73 = 0.0
  IF ((M.EQ.0) .AND. (CND.LE.5E-4)) RETURN
  IF ((M.EQ.1) .AND. (CND.LE.0.02)) RETURN

  DT = T - 15.0
  PBAR = P / 10.0  ! Tekanan dalam bar
  XT = DT
  
  IF (M.EQ.0) THEN
    ! ALGORITMA 1 - Diabaikan di sini, fokus pada M=1
    SAL73 = -999.0 
    RETURN
  END IF
  
  ! ********************************************************
  ! M = 1: SALINITY TO CONDUCTIVITY RATIO (ALGORITMA 2)
  ! ********************************************************
  S_TARGET = CND 
  XR = SQRT(S_TARGET / 35.0) ! Perkiraan awal
  N = 0
  
  ! Iterasi Newton-Raphson
  DO WHILE (N .LT. 10)
    
    ! 1. Hitung Salinitas S(Rt) dari perkiraan saat ini menggunakan fungsi eksternal
    S_CALC = S_FROM_RT(XR, XT)
    DELTA_S = S_TARGET - S_CALC
    
    IF (ABS(DELTA_S) .LT. 1.0E-4) EXIT

    ! 2. Hitung Turunan Parsial dS/dRt menggunakan subroutine eksternal
    CALL GET_DS_DRT(XR, XT, DS_DRT_VAL)
    
    ! Perbarui Rt
    RT = XR**2 + DELTA_S / DS_DRT_VAL
    
    ! Perbarui XR = sqrt(Rt)
    XR = SQRT(RT)

    N = N + 1
  END DO
  
  ! 3. Hitung Rasio Konduktivitas Total (R)
  
  ! Hitung RT35
  RT35_VAL = ( ( ( (1.0031E-9*T - 6.9698E-7)*T + 1.104259E-4)*T + 2.00564E-2)*T &
     &         + 0.6766097)

  ! Hitung Koreksi Tekanan E
  CP_VAL = (C3*PBAR + C2)*PBAR + C1
  A_VAL = -3.107E-3*DT + 0.4215
  B_VAL = 4.464E-4*DT + 3.426E-2
  
  DEN = 1.0 + (B_VAL*RT + A_VAL)*RT 
  E = CP_VAL * PBAR * RT / DEN

  SAL73 = RT * RT35_VAL * (1.0 + E)
  
END FUNCTION SAL73


! ***************************************************************
! FUNGSI PEMBANTU EKSTERNAL (S_FROM_RT) - Menghitung Salinitas
! ***************************************************************
REAL FUNCTION S_FROM_RT(XR, XT)
  IMPLICIT NONE
  REAL, INTENT(IN) :: XR, XT
  
  ! ** Konstanta Koefisien Salinitas (Persamaan 2) **
  REAL, PARAMETER :: A0=0.0080, A1=-0.1692, A2=25.3851, A3=14.0941, A4=-7.0261, A5=2.7081
  ! ** Konstanta Koefisien Koreksi Suhu (Persamaan 2) **
  REAL, PARAMETER :: B0=0.0005, B1=-0.0056, B2=-0.0066, B3=-0.0375, B4=0.0636, B5=-0.0144
  
  S_FROM_RT = ( ( ( ( (A5*XR + A4)*XR + A3)*XR + A2)*XR &
     &                 + A1)*XR + A0 ) &
     &           + (XT/(1.0+0.0162*XT))* ( ( ( ( (B5*XR + B4)*XR &
     &                 + B3)*XR + B2)*XR + B1)*XR + B0 )

END FUNCTION S_FROM_RT


! ***************************************************************
! SUBROUTINE PEMBANTU EKSTERNAL (GET_DS_DRT) - Menghitung Turunan
! ***************************************************************
SUBROUTINE GET_DS_DRT(XR_in, XT_in, DS_DRT_out)
  IMPLICIT NONE
  REAL, INTENT(IN) :: XR_in, XT_in
  REAL, INTENT(OUT) :: DS_DRT_out
  
  REAL :: DS_DXR_VAL
  ! ** Konstanta Koefisien **
  REAL, PARAMETER :: A1=-0.1692, A2=25.3851, A3=14.0941, A4=-7.0261, A5=2.7081
  REAL, PARAMETER :: B1=-0.0056, B2=-0.0066, B3=-0.0375, B4=0.0636, B5=-0.0144
  
  ! Hitung Turunan Parsial dS/dXR
  DS_DXR_VAL = ( (5.0*A5*XR_in + 4.0*A4)*XR_in + 3.0*A3)*XR_in*XR_in + 2.0*A2*XR_in &
     &                 + A1 ) &
     &           + (XT_in/(1.0+0.0162*XT_in))* ( ( (5.0*B5*XR_in + 4.0*B4)*XR_in + 3.0*B3)*XR_in*XR_in &
     &                 + 2.0*B2*XR_in + B1 )

  ! Hitung dS/dRt = (dS/dXR) / (2*XR)
  DS_DRT_out = DS_DXR_VAL / (2.0 * XR_in)
  
END SUBROUTINE GET_DS_DRT