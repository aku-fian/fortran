! ***********************************************************************
! PROGRAM UNTUK MENGHITUNG SPECIFIC VOLUME ANOMALY DAN DENSITY ANOMALY
! BERDASARKAN EQUATION OF STATE 1980 (EOS-80)
! PRACTICAL SALINITY SCALE 1978 (PSS-78)
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: S, T, P, SVAN, SIGMA
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '=================================================='
    PRINT *, '  SPECIFIC VOLUME ANOMALY & DENSITY ANOMALY'
    PRINT *, '  EQUATION OF STATE 1980 (EOS-80)'
    PRINT *, '  PRACTICAL SALINITY SCALE 1978 (PSS-78)'
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
        CALL HITUNG_SVAN(S, T, P, SVAN, SIGMA)
        CALL TAMPIL_HASIL(S, T, P, SVAN, SIGMA)
        
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
            CALL HITUNG_SVAN(S, T, P, SVAN, SIGMA)
            CALL TAMPIL_HASIL(S, T, P, SVAN, SIGMA)
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
! SUBROUTINE UNTUK MENGHITUNG SPECIFIC VOLUME ANOMALY
! BERDASARKAN EOS-80
! ***********************************************************************
SUBROUTINE HITUNG_SVAN(S, T, P0, SVAN, SIGMA)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P0
    REAL, INTENT(OUT) :: SVAN, SIGMA
    REAL :: P, SR
    REAL :: R1, R2, R3, R4
    REAL :: A, B, C, D, E, KW, K, K0, K35
    REAL :: GAM, PK, SIG
    REAL :: V350P, SVA
    REAL :: DR350, DR35P, DVAN
    
! KONSTANTA
    REAL, PARAMETER :: R3500 = 1028.1063
    REAL, PARAMETER :: DR350_CONST = 28.106331
    R4 = 4.8314E-4
    
! KONVERSI PRESSURE KE BARS
    P = P0 / 10.0
    
! SQRT SALINITY
    SR = SQRT(ABS(S))
    
! ========== PURE WATER DENSITY AT ATMOSPHERIC PRESSURE ==========
! Bigg P.H., (1967) Br. J. Applied Physics 8 pp 521-537
    R1 = ((((6.536332E-9*T - 1.120083E-6)*T + 1.001685E-4)*T &
          - 9.095290E-3)*T + 6.793952E-2)*T - 28.263737
    
! ========== SEAWATER DENSITY ATM PRESS ==========
! R2 = A in notation
    R2 = (((5.3875E-9*T - 8.2467E-7)*T + 7.6438E-5)*T &
         - 4.0899E-3)*T + 8.24493E-1
    
! R3 = B in notation
    R3 = (-1.6546E-6*T + 1.0227E-4)*T - 5.72466E-3
    
! INTERNATIONAL ONE-ATMOSPHERE EQUATION OF STATE
    SIG = (R4*S + R3*SR + R2)*S + R1
    
! SPECIFIC VOLUME AT ATMOSPHERIC PRESSURE
    V350P = 1.0 / R3500
    SVA = -SIG * V350P / (R3500 + SIG)
    SIGMA = SIG + DR350_CONST
    
! SCALE SPECIFIC VOL. ANOMALY TO NORMALLY REPORTED UNITS
    SVAN = SVA * 1.0E+8
    
    IF (P == 0.0) RETURN
    
! ========== HIGH PRESSURE EQUATION OF STATE ==========
! Millero et al., 1980 DSR 27A, pp 255-264
    
! COMPUTE COMPRESSION TERMS
    E = (9.1697E-10*T + 2.0816E-8)*T - 9.9348E-7
    B = (5.2787E-8*T - 6.12293E-6)*T + 3.47718E-5
    B = B + E*S
    
    D = 1.91075E-4
    C = (-1.6078E-6*T - 1.0981E-5)*T + 2.2838E-3
    A = ((-5.77905E-7*T + 1.16092E-4)*T + 1.43713E-3)*T - 0.1194975
    A = (D*SR + C)*S + A
    
    B = (-5.3009E-4*T + 1.6483E-2)*T + 7.944E-2
    A = ((-6.1670E-5*T + 1.09987E-2)*T - 0.603459)*T + 54.6746
    KW = (((-5.155288E-5*T + 1.360477E-2)*T - 2.327105)*T &
         + 148.4206)*T - 1930.06
    K0 = (B*SR + A)*S + KW
    
! Note: Reusing variables A and B for different purposes
! EVALUATE PRESSURE POLYNOMIAL
! K EQUALS THE SECANT BULK MODULUS OF SEAWATER
! DK = K(S,T,P) - K(35,0,P)
! K35 = K(35,0,P)
    K = (B*P + A)*P + K0
    K35 = (5.03217E-5*P + 3.359406)*P + 21582.27
    GAM = P / K35
    PK = 1.0 - GAM
    SVA = SVA*PK + (V350P + SVA)*P*K/(K35*(K35 + K))
    
! SCALE SPECIFIC VOL. ANOMALY
    SVAN = SVA * 1.0E+8
    V350P = V350P * PK
    
! ========== COMPUTE DENSITY ANOMALY ==========
! 1) DR350: DENSITY ANOMALY AT 35 (PSS-78), 0 DEG C AND 0 DECIBARS
! 2) DR35P: DENSITY ANOMALY 35 (PSS-78), 0 DEG C, PRES. VARIATION
! 3) DVAN: DENSITY ANOMALY VARIATIONS INVOLVING SPECIFIC VOL. ANOMALY
    
    DR35P = GAM / V350P
    DVAN = SVA / (V350P * (V350P + SVA))
    SIGMA = DR350_CONST + DR35P - DVAN
    
END SUBROUTINE HITUNG_SVAN

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(S, T, P, SVAN, SIGMA)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P, SVAN, SIGMA
    
    PRINT *, ''
    PRINT *, '========== HASIL PERHITUNGAN =========='
    PRINT *, 'SALINITY (PSS-78)         : ', S
    PRINT *, 'TEMPERATURE (IPTS-68)     : ', T, ' DEG C'
    PRINT *, 'PRESSURE                  : ', P, ' DECIBARS'
    PRINT *, 'SPECIFIC VOLUME ANOMALY   : ', SVAN, ' x 1.0E-8 m^3/kg'
    PRINT *, 'DENSITY ANOMALY (SIGMA)   : ', SIGMA, ' kg/m^3'
    PRINT *, '======================================='
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: S, T, P, SVAN, SIGMA
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
    CALL HITUNG_SVAN(S, T, P, SVAN, SIGMA)
    CALL TAMPIL_HASIL(S, T, P, SVAN, SIGMA)
    PRINT *, ''
    
    GOTO 300
    
400 CONTINUE
    CLOSE(10)
    
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
END SUBROUTINE READ_FILE