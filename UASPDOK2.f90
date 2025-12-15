! ***********************************************************************
! PROGRAM UTAMA UNTUK KONVERSI SALINITAS KE KONDUKTIVITAS
! MENGGUNAKAN PRACTICAL SALINITY SCALE 1978 (PSS-78)
! METODE: NEWTON-RAPHSON ITERATION
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: S, T, P, CND
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '=================================================='
    PRINT *, '   KONVERSI SALINITAS KE KONDUKTIVITAS'
    PRINT *, '   PRACTICAL SALINITY SCALE 1978 (PSS-78)'
    PRINT *, '   METODE: NEWTON-RAPHSON ITERATION'
    PRINT *, '=================================================='
    PRINT *, ''
    
! LOOP UTAMA PROGRAM
100 CONTINUE
    
! MENU PILIHAN INPUT
    PRINT *, 'PILIH METODE INPUT:'
    PRINT *, '1. INPUT MANUAL (SATU DATA)'
    PRINT *, '2. INPUT MANUAL (MULTIPLE DATA)'
    PRINT *, '3. READ DARI FILE'
    PRINT *, '0. KELUAR'
    PRINT *, ''
    PRINT *, 'MASUKKAN PILIHAN (0-3): '
    READ(*,*) PILIHAN
    PRINT *, ''
    
! PEMILIHAN METODE INPUT MENGGUNAKAN IF
    IF (PILIHAN == 0) THEN
        PRINT *, 'TERIMA KASIH TELAH MENGGUNAKAN PROGRAM INI'
        STOP
        
    ELSE IF (PILIHAN == 1) THEN
! INPUT MANUAL SATU DATA
        CALL INPUT_MANUAL(S, T, P)
        CALL HITUNG_CND(S, T, P, CND)
        CALL TAMPIL_HASIL(S, T, P, CND)
        
    ELSE IF (PILIHAN == 2) THEN
! INPUT MANUAL MULTIPLE DATA
        PRINT *, 'BERAPA JUMLAH DATA YANG AKAN DIINPUT? '
        READ(*,*) N
        PRINT *, ''
        
        IF (N <= 0) THEN
            PRINT *, 'ERROR: JUMLAH DATA HARUS > 0'
            GOTO 100
        END IF
        
! LOOP UNTUK MULTIPLE INPUT
        DO I = 1, N
            PRINT *, '--- DATA KE-', I, ' ---'
            CALL INPUT_MANUAL(S, T, P)
            CALL HITUNG_CND(S, T, P, CND)
            CALL TAMPIL_HASIL(S, T, P, CND)
            PRINT *, ''
        END DO
        
    ELSE IF (PILIHAN == 3) THEN
! READ DATA DARI FILE
        CALL READ_FILE()
        
    ELSE
        PRINT *, 'PILIHAN TIDAK VALID!'
        PRINT *, ''
        GOTO 100
    END IF
    
! TANYA APAKAH INGIN LANJUT
    PRINT *, ''
    PRINT *, 'INGIN MENGHITUNG LAGI? (Y/N): '
    READ(*,'(A)') LANJUT
    PRINT *, ''
    
    IF (LANJUT == 'Y' .OR. LANJUT == 'y') THEN
        GOTO 100
    END IF
    
    PRINT *, 'TERIMA KASIH TELAH MENGGUNAKAN PROGRAM INI'
    STOP
END PROGRAM MAIN

! ***********************************************************************
! SUBROUTINE UNTUK INPUT MANUAL
! ***********************************************************************
SUBROUTINE INPUT_MANUAL(S, T, P)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: S, T, P
    
    PRINT *, 'MASUKKAN SALINITY (S) PSS-78: '
    READ(*,*) S
    
    PRINT *, 'MASUKKAN TEMPERATURE (T) DEG CELSIUS: '
    READ(*,*) T
    
    PRINT *, 'MASUKKAN PRESSURE (P) DECIBARS: '
    READ(*,*) P
    
END SUBROUTINE INPUT_MANUAL

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG KONDUKTIVITAS DARI SALINITAS
! Paper formula: R = r_t * R_t * R_p
! Dimana R_t harus dicari dengan iterasi Newton-Raphson
! ***********************************************************************
SUBROUTINE HITUNG_CND(S, T, P, CND)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P
    REAL, INTENT(OUT) :: CND
    REAL :: RT, RT35, R, RTT
    REAL :: SI, DELS, DSAL_DRT
    REAL :: AT, BT, CP
    INTEGER :: N
    INTEGER, PARAMETER :: MAX_ITER = 10
    REAL, PARAMETER :: TOLERANCE = 1.0E-4
    
! CHECK UNTUK SALINITY TERLALU KECIL
    IF (S <= 0.02) THEN
        CND = 0.0
        RETURN
    END IF
    
! HITUNG RT35 - r_t(T) = C(35,T,0)/C(35,15,0)
    CALL CALC_RT35(T, RT35)
    
! FIRST APPROXIMATION: R_t = S/35
! Dalam kode ini RT = R_t (bukan sqrt!)
    RT = S / 35.0
    
! NEWTON-RAPHSON ITERATION UNTUK MENDAPATKAN R_t
! Dari formula salinitas S(R_t, T), cari R_t yang menghasilkan S
    N = 0
    DO WHILE (N < MAX_ITER)
! HITUNG SALINITY DARI RT SAAT INI
        CALL SAL_FROM_RT(RT, T, SI)
        
! HITUNG SELISIH
        DELS = ABS(SI - S)
        
! CHECK CONVERGENCE
        IF (DELS <= TOLERANCE) EXIT
        
! HITUNG TURUNAN dS/dRT
        CALL CALC_DSAL(RT, T, DSAL_DRT)
        
! Cek jika turunan terlalu kecil
        IF (ABS(DSAL_DRT) < 1.0E-10) THEN
            PRINT *, 'WARNING: Derivative too small'
            EXIT
        END IF
        
! UPDATE RT MENGGUNAKAN NEWTON-RAPHSON
! RT_new = RT_old + (S - S_calc) / (dS/dRT)
        RT = RT + (S - SI) / DSAL_DRT
        
! Pastikan RT tetap positif dan masuk akal
        IF (RT < 0.0) RT = 0.01
        IF (RT > 2.0) RT = 2.0
        
        N = N + 1
    END DO
    
! SEKARANG RT adalah R_t = C(S,T,0)/C(35,T,0)
! HITUNG R dari: R = r_t * R_t * R_p
    
    IF (P == 0.0) THEN
! JIKA PRESSURE = 0, MAKA R_p = 1
! R = r_t * R_t = RT35 * RT
        R = RT35 * RT
    ELSE
! UNTUK PRESSURE > 0: SOLVE QUADRATIC EQUATION
! R = r_t * R_t * R_p
! Dimana R_p = 1 + C*P/(B + A*R)
        
        CALL CALC_PRESSURE_COEFF(T, P, AT, BT, CP)
        RTT = RT35 * RT
        
! Solve: AT*R^2 + (BT - RTT*AT)*R - RTT*(BT + CP) = 0
        CALL SOLVE_QUADRATIC(AT, BT, CP, RTT, R)
    END IF
    
! CONDUCTIVITY RATIO RETURN
    CND = R
    
! TAMPILKAN INFO ITERASI
    IF (N >= MAX_ITER) THEN
        PRINT *, 'WARNING: Maximum iterations reached'
    ELSE
        PRINT *, 'CONVERGED in ', N, ' iterations'
    END IF
    
END SUBROUTINE HITUNG_CND

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG KOEFISIEN PRESSURE
! ***********************************************************************
SUBROUTINE CALC_PRESSURE_COEFF(T, P, AT, BT, CP)
    IMPLICIT NONE
    REAL, INTENT(IN) :: T, P
    REAL, INTENT(OUT) :: AT, BT, CP
    REAL :: E1, E2, E3
    REAL :: D1, D2, D3, D4
    
! KONSTANTA UNTUK RP (Equation 4)
    E1 = 2.070E-5
    E2 = -6.370E-10
    E3 = 3.989E-15
    D1 = 3.426E-2
    D2 = 4.464E-4
    D3 = 4.215E-1
    D4 = -3.107E-3
    
! C(P) = e1*P + e2*P^2 + e3*P^3
    CP = ((E3*P + E2)*P + E1) * P
    
! B(T) = 1 + d1*T + d2*T^2
    BT = 1.0 + D1*T + D2*T*T
    
! A(T) = d3 + d4*T
    AT = D3 + D4*T
    
END SUBROUTINE CALC_PRESSURE_COEFF

! ***********************************************************************
! SUBROUTINE UNTUK MENYELESAIKAN PERSAMAAN KUADRATIK
! ***********************************************************************
SUBROUTINE SOLVE_QUADRATIC(AT, BT, CP, RTT, R)
    IMPLICIT NONE
    REAL, INTENT(IN) :: AT, BT, CP, RTT
    REAL, INTENT(OUT) :: R
    REAL :: A_COEF, B_COEF, C_COEF
    REAL :: DISCRIMINANT
    
! Persamaan: AT*R^2 + (BT - RTT*AT)*R - RTT*(BT + CP) = 0
    A_COEF = AT
    B_COEF = BT - RTT * AT
    C_COEF = -RTT * (BT + CP)
    
! Hitung diskriminan
    DISCRIMINANT = B_COEF * B_COEF - 4.0 * A_COEF * C_COEF
    
    IF (DISCRIMINANT < 0.0) THEN
        PRINT *, 'ERROR: Negative discriminant!'
        R = RTT
        RETURN
    END IF
    
! Gunakan rumus kuadratik: R = (-B + sqrt(B^2 - 4AC)) / (2A)
! Ambil akar positif
    R = (-B_COEF + SQRT(DISCRIMINANT)) / (2.0 * A_COEF)
    
END SUBROUTINE SOLVE_QUADRATIC

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG r_t = C(35,T,0)/C(35,15,0)
! ***********************************************************************
SUBROUTINE CALC_RT35(T, RT35)
    IMPLICIT NONE
    REAL, INTENT(IN) :: T
    REAL, INTENT(OUT) :: RT35
    REAL :: C0, C1, C2, C3, C4
    
! KONSTANTA UNTUK RT35 (Equation 3)
    C0 = 0.6766097
    C1 = 2.00564E-2
    C2 = 1.104259E-4
    C3 = -6.9698E-7
    C4 = 1.0031E-9
    
    RT35 = ((((C4*T + C3)*T + C2)*T + C1)*T + C0)
    
END SUBROUTINE CALC_RT35

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG SALINITY DARI RT
! S(RT,T) - Equation (1) dan (2)
! PENTING: RT di sini adalah R_t (ratio penuh), BUKAN sqrt(R_t)!
! Formula menggunakan XR = sqrt(RT) untuk ekspansi polynomial
! ***********************************************************************
SUBROUTINE SAL_FROM_RT(RT, T, S)
    IMPLICIT NONE
    REAL, INTENT(IN) :: RT, T
    REAL, INTENT(OUT) :: S
    REAL :: A0, A1, A2, A3, A4, A5
    REAL :: B0, B1, B2, B3, B4, B5
    REAL :: K, DT, DS, XR
    
! KONSTANTA UNTUK PERHITUNGAN SALINITAS
    A0 = 0.0080
    A1 = -0.1692
    A2 = 25.3851
    A3 = 14.0941
    A4 = -7.0261
    A5 = 2.7081
    
    B0 = 0.0005
    B1 = -0.0056
    B2 = -0.0066
    B3 = -0.0375
    B4 = 0.0636
    B5 = -0.0144
    K = 0.0162
    
    DT = T - 15.0
    
! XR = sqrt(R_t) untuk formula polynomial
    XR = SQRT(ABS(RT))
    
! HITUNG SALINITAS
! S = a0 + a1*sqrt(Rt) + a2*Rt + a3*Rt^1.5 + a4*Rt^2 + a5*Rt^2.5
! S = a0 + a1*XR + a2*XR^2 + a3*XR^3 + a4*XR^4 + a5*XR^5
    S = A0 + A1*XR + A2*XR*XR + A3*XR*XR*XR + A4*XR*XR*XR*XR + &
        A5*XR*XR*XR*XR*XR
    
! KOREKSI DELTA S
    DS = (DT / (1.0 + K*DT)) * &
         (B0 + B1*XR + B2*XR*XR + B3*XR*XR*XR + B4*XR*XR*XR*XR + &
          B5*XR*XR*XR*XR*XR)
    
    S = S + DS
    
END SUBROUTINE SAL_FROM_RT

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG dS/dRT (TURUNAN)
! Karena S = f(sqrt(RT)), gunakan chain rule
! ***********************************************************************
SUBROUTINE CALC_DSAL(RT, T, DSAL_DRT)
    IMPLICIT NONE
    REAL, INTENT(IN) :: RT, T
    REAL, INTENT(OUT) :: DSAL_DRT
    REAL :: A0, A1, A2, A3, A4, A5
    REAL :: B0, B1, B2, B3, B4, B5
    REAL :: K, DT, XR, DSAL_DXR
    
! KONSTANTA
    A0 = 0.0080
    A1 = -0.1692
    A2 = 25.3851
    A3 = 14.0941
    A4 = -7.0261
    A5 = 2.7081
    
    B0 = 0.0005
    B1 = -0.0056
    B2 = -0.0066
    B3 = -0.0375
    B4 = 0.0636
    B5 = -0.0144
    K = 0.0162
    
    DT = T - 15.0
    XR = SQRT(ABS(RT))
    
    IF (RT <= 1.0E-10) THEN
        DSAL_DRT = A1 * 0.5 / SQRT(1.0E-10)
        RETURN
    END IF
    
! dS/dXR dimana S = a0 + a1*XR + a2*XR^2 + a3*XR^3 + a4*XR^4 + a5*XR^5
! dS/dXR = a1 + 2*a2*XR + 3*a3*XR^2 + 4*a4*XR^3 + 5*a5*XR^4
    DSAL_DXR = A1 + 2.0*A2*XR + 3.0*A3*XR*XR + 4.0*A4*XR*XR*XR + &
               5.0*A5*XR*XR*XR*XR
    
    DSAL_DXR = DSAL_DXR + (DT/(1.0 + K*DT)) * &
               (B1 + 2.0*B2*XR + 3.0*B3*XR*XR + 4.0*B4*XR*XR*XR + &
                5.0*B5*XR*XR*XR*XR)
    
! Chain rule: dS/dRT = dS/dXR * dXR/dRT
! dXR/dRT = 1/(2*sqrt(RT)) = 1/(2*XR)
    DSAL_DRT = DSAL_DXR * 0.5 / XR
    
END SUBROUTINE CALC_DSAL

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(S, T, P, CND)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P, CND
    
    PRINT *, ''
    PRINT *, '========== HASIL PERHITUNGAN =========='
    PRINT *, 'SALINITY (PSS-78)  : ', S
    PRINT *, 'TEMPERATURE        : ', T, ' DEG C'
    PRINT *, 'PRESSURE           : ', P, ' DECIBARS'
    PRINT *, 'CONDUCTIVITY RATIO : ', CND
    PRINT *, '======================================='
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: S, T, P, CND
    INTEGER :: IOS, COUNT
    CHARACTER(50) :: FILENAME
    
    PRINT *, 'MASUKKAN NAMA FILE (contoh: data.txt): '
    READ(*,'(A)') FILENAME
    
! BUKA FILE
    OPEN(UNIT=10, FILE=FILENAME, STATUS='OLD', IOSTAT=IOS)
    
    IF (IOS /= 0) THEN
        PRINT *, 'ERROR: FILE TIDAK DAPAT DIBUKA!'
        PRINT *, 'PASTIKAN FILE ADA DI DIREKTORI YANG SAMA'
        RETURN
    END IF
    
    PRINT *, ''
    PRINT *, 'MEMBACA DATA DARI FILE: ', TRIM(FILENAME)
    PRINT *, ''
    
    COUNT = 0
    
! LOOP MEMBACA FILE HINGGA END OF FILE
300 CONTINUE
    READ(10, *, IOSTAT=IOS) S, T, P
    
    IF (IOS < 0) THEN
! END OF FILE
        GOTO 400
    ELSE IF (IOS > 0) THEN
! ERROR READING
        PRINT *, 'ERROR MEMBACA DATA PADA BARIS', COUNT + 1
        GOTO 300
    END IF
    
    COUNT = COUNT + 1
    PRINT *, '--- DATA KE-', COUNT, ' ---'
    CALL HITUNG_CND(S, T, P, CND)
    CALL TAMPIL_HASIL(S, T, P, CND)
    PRINT *, ''
    
    GOTO 300
    
400 CONTINUE
    CLOSE(10)
    
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
END SUBROUTINE READ_FILE