! ***********************************************************************
! PROGRAM UNTUK MENGHITUNG POTENTIAL TEMPERATURE
! MENGGUNAKAN METODE RUNGE-KUTTA 4TH ORDER
! BRYDEN (1973) & FOFONOFF (1977)
! UNESCO TECHNICAL PAPERS NO. 44 (1983)
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: S, T0, P0, PR, THETA
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '======================================================'
    PRINT *, '        POTENTIAL TEMPERATURE'
    PRINT *, '     RUNGE-KUTTA 4TH ORDER METHOD'
    PRINT *, '    BRYDEN (1973) & FOFONOFF (1977)'
    PRINT *, '       UNESCO TECH PAPERS NO. 44 (1983)'
    PRINT *, '======================================================'
    PRINT *, ''
    
! LOOP UTAMA PROGRAM
100 CONTINUE
    
! MENU PILIHAN INPUT
    PRINT *, 'PILIH METODE INPUT:'
    PRINT *, '1. INPUT MANUAL (SATU DATA)'
    PRINT *, '2. INPUT MANUAL (MULTIPLE DATA)'
    PRINT *, '3. READ DARI FILE'
    PRINT *, '4. TAMPILKAN TABEL LENGKAP'
    PRINT *, '0. KELUAR'
    PRINT *, ''
    PRINT *, 'MASUKKAN PILIHAN (0-4): '
    READ(*,*) PILIHAN
    PRINT *, ''
    
! PEMILIHAN METODE INPUT MENGGUNAKAN IF
    IF (PILIHAN == 0) THEN
        PRINT *, 'TERIMA KASIH TELAH MENGGUNAKAN PROGRAM INI'
        STOP
        
    ELSE IF (PILIHAN == 1) THEN
! INPUT MANUAL SATU DATA
        CALL INPUT_MANUAL(S, T0, P0, PR)
        CALL HITUNG_THETA(S, T0, P0, PR, THETA)
        CALL TAMPIL_HASIL(S, T0, P0, PR, THETA)
        
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
            CALL INPUT_MANUAL(S, T0, P0, PR)
            CALL HITUNG_THETA(S, T0, P0, PR, THETA)
            CALL TAMPIL_HASIL(S, T0, P0, PR, THETA)
            PRINT *, ''
        END DO
        
    ELSE IF (PILIHAN == 3) THEN
! READ DATA DARI FILE
        CALL READ_FILE()
        
    ELSE IF (PILIHAN == 4) THEN
! TAMPILKAN TABEL LENGKAP
        CALL TAMPIL_TABEL()
        
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
SUBROUTINE INPUT_MANUAL(S, T0, P0, PR)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: S, T0, P0, PR
    
    PRINT *, 'MASUKKAN SALINITY (S) PSS-78: '
    READ(*,*) S
    
! VALIDASI SALINITY
    IF (S < 0.0 .OR. S > 40.0) THEN
        PRINT *, 'WARNING: SALINITY DI LUAR RANGE VALID (0-40)'
    END IF
    
    PRINT *, 'MASUKKAN IN-SITU TEMPERATURE (T0) DEG CELSIUS: '
    READ(*,*) T0
    
! VALIDASI TEMPERATURE
    IF (T0 < -2.0 .OR. T0 > 40.0) THEN
        PRINT *, 'WARNING: TEMPERATURE DI LUAR RANGE VALID (-2 to 40)'
    END IF
    
    PRINT *, 'MASUKKAN IN-SITU PRESSURE (P0) DECIBARS: '
    READ(*,*) P0
    
! VALIDASI PRESSURE
    IF (P0 < 0.0 .OR. P0 > 10000.0) THEN
        PRINT *, 'WARNING: PRESSURE DI LUAR RANGE VALID (0-10000)'
    END IF
    
    PRINT *, 'MASUKKAN REFERENCE PRESSURE (PR) DECIBARS: '
    READ(*,*) PR
    
! VALIDASI REFERENCE PRESSURE
    IF (PR < 0.0 .OR. PR > 10000.0) THEN
        PRINT *, 'WARNING: REFERENCE PRESSURE DI LUAR RANGE VALID (0-10000)'
    END IF
    
END SUBROUTINE INPUT_MANUAL

! ***********************************************************************
! FUNCTION ATG - ADIABATIC TEMPERATURE GRADIENT
! DIPERLUKAN UNTUK PERHITUNGAN POTENTIAL TEMPERATURE
! ***********************************************************************
REAL FUNCTION ATG(S, T, P)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P
    REAL :: DS
    
    DS = S - 35.0
    
    ATG = (((-2.1687E-16*T + 1.8676E-14)*T - 4.6206E-13)*P + &
           ((2.7759E-12*T - 1.1351E-10)*DS + &
           (((-5.4481E-14*T + 8.733E-12)*T - 6.7795E-10)*T + 1.8741E-8)))*P + &
           (-4.2393E-8*T + 1.8932E-6)*DS + &
           (((6.6228E-10*T - 6.836E-8)*T + 8.5258E-6)*T + 3.5803E-5)
    
    RETURN
END FUNCTION ATG

! ***********************************************************************
! FUNCTION THETA - POTENTIAL TEMPERATURE
! MENGGUNAKAN RUNGE-KUTTA 4TH ORDER INTEGRATION
! ***********************************************************************
REAL FUNCTION THETA(S, T0, P0, PR)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T0, P0, PR
    REAL :: P, T, H, XK, Q
    REAL :: ATG  ! Function declaration
    
! SET-UP INTERMEDIATE TEMPERATURE AND PRESSURE VARIABLES
    P = P0
    T = T0
    H = PR - P
    
! RUNGE-KUTTA 4TH ORDER INTEGRATION
! FIRST STEP
    XK = H * ATG(S, T, P)
    T = T + 0.5 * XK
    Q = XK
    P = P + 0.5 * H
    
! SECOND STEP
    XK = H * ATG(S, T, P)
    T = T + 0.29289322 * (XK - Q)
    Q = 0.58578644 * XK + 0.121320344 * Q
    
! THIRD STEP
    XK = H * ATG(S, T, P)
    T = T + 1.707106781 * (XK - Q)
    Q = 3.414213562 * XK - 4.121320344 * Q
    P = P + 0.5 * H
    
! FOURTH STEP
    XK = H * ATG(S, T, P)
    THETA = T + (XK - 2.0 * Q) / 6.0
    
    RETURN
END FUNCTION THETA

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG POTENTIAL TEMPERATURE
! ***********************************************************************
SUBROUTINE HITUNG_THETA(S, T0, P0, PR, THETA_RESULT)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T0, P0, PR
    REAL, INTENT(OUT) :: THETA_RESULT
    REAL :: THETA  ! Function declaration
    
    THETA_RESULT = THETA(S, T0, P0, PR)
    
END SUBROUTINE HITUNG_THETA

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(S, T0, P0, PR, THETA)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T0, P0, PR, THETA
    REAL :: TEMP_DIFF
    
    TEMP_DIFF = T0 - THETA
    
    PRINT *, ''
    PRINT *, '================= HASIL PERHITUNGAN ================='
    PRINT 100, 'SALINITY (S)         : ', S, ' PSS-78'
    PRINT 100, 'IN-SITU TEMP (T0)    : ', T0, ' DEG CELSIUS'
    PRINT 100, 'IN-SITU PRESS (P0)   : ', P0, ' DECIBARS'
    PRINT 100, 'REFERENCE PRESS (PR) : ', PR, ' DECIBARS'
    PRINT *, '-----------------------------------------------------'
    PRINT 100, 'POTENTIAL TEMP (Θ)   : ', THETA, ' DEG CELSIUS'
    PRINT 100, 'TEMP DIFFERENCE      : ', TEMP_DIFF, ' DEG CELSIUS'
    PRINT *, '====================================================='
    
100 FORMAT(A, F10.5, A)
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: S, T0, P0, PR, THETA_RESULT
    INTEGER :: IOS, COUNT
    CHARACTER(50) :: FILENAME
    REAL :: THETA  ! Function declaration
    
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
    
! PRINT HEADER TABEL
    PRINT *, '=================================================================================================='
    PRINT *, ' NO | SALINITY | IN-SITU T | IN-SITU P | REF PRESS | POTENTIAL T | TEMP DIFF |    STATUS'
    PRINT *, '    | (PSS-78) | (DEG C)   | (DECIBARS)| (DECIBARS)| (DEG C)     | (DEG C)   |'
    PRINT *, '=================================================================================================='
    
    COUNT = 0
    
! LOOP MEMBACA FILE HINGGA END OF FILE
300 CONTINUE
    READ(10, *, IOSTAT=IOS) S, T0, P0, PR
    
    IF (IOS < 0) THEN
! END OF FILE
        GOTO 400
    ELSE IF (IOS > 0) THEN
! ERROR READING
        PRINT *, 'ERROR MEMBACA DATA PADA BARIS', COUNT + 1
        GOTO 300
    END IF
    
    COUNT = COUNT + 1
    THETA_RESULT = THETA(S, T0, P0, PR)
    
! PRINT DATA DALAM FORMAT TABEL
    PRINT 200, COUNT, S, T0, P0, PR, THETA_RESULT, T0-THETA_RESULT, 'OK'
    
    GOTO 300
    
400 CONTINUE
    PRINT *, '=================================================================================================='
    CLOSE(10)
    
    PRINT *, ''
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
200 FORMAT(I4, ' |', F8.2, '  |', F9.2, '  |', F9.1, '  |', F9.1, '  |', F11.5, '  |', F9.5, '  | ', A)
    
END SUBROUTINE READ_FILE

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN TABEL LENGKAP
! SESUAI DENGAN TABEL DI UNESCO TECH PAPERS NO. 44
! ***********************************************************************
SUBROUTINE TAMPIL_TABEL()
    IMPLICIT NONE
    REAL :: S, T, P, THETA_VALUE
    INTEGER :: I, J, K
    REAL :: T_ARRAY(5), P_ARRAY(11)
    REAL :: THETA  ! Function declaration
    INTEGER :: SALINITY
    REAL :: PR
    
! ARRAY TEMPERATURE
    DATA T_ARRAY /0.0, 10.0, 20.0, 30.0, 40.0/
    
! ARRAY PRESSURE
    DATA P_ARRAY /0.0, 1000.0, 2000.0, 3000.0, 4000.0, 5000.0, &
                   6000.0, 7000.0, 8000.0, 9000.0, 10000.0/
    
! REFERENCE PRESSURE = 0 (ATMOSPHERIC PRESSURE)
    PR = 0.0
    
    PRINT *, ''
    PRINT *, 'CATATAN: Tabel ini menghitung POTENTIAL TEMPERATURE'
    PRINT *, '         dengan REFERENCE PRESSURE = 0 decibars'
    PRINT *, ''
    
! LOOP UNTUK SALINITY 25, 30, 35, 40
    DO K = 25, 40, 5
        S = REAL(K)
        SALINITY = K
        
        PRINT *, ''
        PRINT *, '============================================================================='
        PRINT *, '      POTENTIAL TEMPERATURE Θ [DEG C] (Ref. Pres. = 0.0 dbar)'
        PRINT 500, '                        SALINITY: ', SALINITY
        PRINT *, '============================================================================='
        PRINT *, ''
        PRINT *, 'PRESSURE  |         IN-SITU TEMPERATURE (DEG C) IPTS-68'
        PRINT *, 'DECIBARS  |      0        10        20        30        40'
        PRINT *, '----------|------------------------------------------------------------'
        
! LOOP UNTUK SETIAP PRESSURE
        DO J = 1, 11
            P = P_ARRAY(J)
            
! PRINT PRESSURE VALUE
            WRITE(*, '(F9.0, A)', ADVANCE='NO') P, '  |'
            
! LOOP UNTUK SETIAP TEMPERATURE
            DO I = 1, 5
                T = T_ARRAY(I)
                
! HITUNG POTENTIAL TEMPERATURE
                THETA_VALUE = THETA(S, T, P, PR)
                
! PRINT VALUE DENGAN FORMAT
                WRITE(*, '(F9.5)', ADVANCE='NO') THETA_VALUE
            END DO
            
! NEW LINE
            PRINT *, ''
            
        END DO
        
        PRINT *, '----------|------------------------------------------------------------'
        
    END DO
    
    PRINT *, ''
    PRINT *, 'CHECKVALUE: THETA = 36.89073 DEG C'
    PRINT *, '            FOR S=40, T0=40 DEG C, P0=10000 DECIBARS, PR=0 DECIBARS'
    PRINT *, ''
    PRINT *, 'REFERENCES:'
    PRINT *, '  BRYDEN, H., 1973, DEEP-SEA RES., 20, 401-408'
    PRINT *, '  FOFONOFF, N., 1977, DEEP-SEA RES., 24, 489-491'
    PRINT *, ''
    PRINT *, 'METHOD: RUNGE-KUTTA 4TH ORDER INTEGRATION'
    PRINT *, '        Integration error < 0.1 x 10^-3 DEG C'
    PRINT *, ''
    
500 FORMAT(A, I2)
    
END SUBROUTINE TAMPIL_TABEL
