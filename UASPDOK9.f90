! ***********************************************************************
! PROGRAM UNTUK MENGHITUNG SOUND SPEED IN SEAWATER
! CHEN AND MILLERO (1977)
! UNESCO TECHNICAL PAPERS NO. 44 (1983)
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: S, T, P0, SVEL_RESULT
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '======================================================'
    PRINT *, '        SOUND SPEED IN SEAWATER'
    PRINT *, '          CHEN AND MILLERO (1977)'
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
        CALL INPUT_MANUAL(S, T, P0)
        CALL HITUNG_SVEL(S, T, P0, SVEL_RESULT)
        CALL TAMPIL_HASIL(S, T, P0, SVEL_RESULT)
        
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
            CALL INPUT_MANUAL(S, T, P0)
            CALL HITUNG_SVEL(S, T, P0, SVEL_RESULT)
            CALL TAMPIL_HASIL(S, T, P0, SVEL_RESULT)
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
SUBROUTINE INPUT_MANUAL(S, T, P0)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: S, T, P0
    
    PRINT *, 'MASUKKAN SALINITY (S) PSS-78: '
    READ(*,*) S
    
! VALIDASI SALINITY
    IF (S < 0.0 .OR. S > 40.0) THEN
        PRINT *, 'WARNING: SALINITY DI LUAR RANGE VALID (0-40)'
    END IF
    
    PRINT *, 'MASUKKAN TEMPERATURE (T) DEG CELSIUS: '
    READ(*,*) T
    
! VALIDASI TEMPERATURE
    IF (T < 0.0 .OR. T > 40.0) THEN
        PRINT *, 'WARNING: TEMPERATURE DI LUAR RANGE VALID (0-40)'
    END IF
    
    PRINT *, 'MASUKKAN PRESSURE (P0) DECIBARS: '
    READ(*,*) P0
    
! VALIDASI PRESSURE
    IF (P0 < 0.0 .OR. P0 > 10000.0) THEN
        PRINT *, 'WARNING: PRESSURE DI LUAR RANGE VALID (0-10000)'
    END IF
    
END SUBROUTINE INPUT_MANUAL

! ***********************************************************************
! FUNCTION SVEL - SOUND SPEED IN SEAWATER
! CHEN AND MILLERO (1977), JASA, 62, 1129-1135
! ***********************************************************************
REAL FUNCTION SVEL(S, T, P0)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P0
    REAL :: P, SR, D, B1, B0, B, A3, A2, A1, A0, A
    REAL :: C3, C2, C1, C0, C
    
! KONSTANTA UNTUK FORMULA
! S**2 TERM
    REAL, PARAMETER :: D0 = 1.727E-3, D1 = -7.9836E-6
    
! S**3/2 TERM
    REAL, PARAMETER :: B00 = -1.922E-2, B01 = -4.42E-5
    REAL, PARAMETER :: B10 = 7.3637E-5, B11 = 1.7945E-7
    
! S**1 TERM COEFFICIENTS
    REAL, PARAMETER :: A00 = 1.389, A01 = -1.262E-2, A02 = 7.164E-5
    REAL, PARAMETER :: A03 = 2.006E-6, A04 = -3.21E-8
    REAL, PARAMETER :: A10 = 9.4742E-5, A11 = -1.2580E-5
    REAL, PARAMETER :: A12 = -6.4885E-8, A13 = 1.0507E-8
    REAL, PARAMETER :: A14 = -2.0122E-10
    REAL, PARAMETER :: A20 = -3.9064E-7, A21 = 9.1041E-9
    REAL, PARAMETER :: A22 = -1.6002E-10, A23 = 7.988E-12
    REAL, PARAMETER :: A30 = 1.100E-10, A31 = 6.649E-12, A32 = -3.389E-13
    
! S**0 TERM COEFFICIENTS
    REAL, PARAMETER :: C00 = 1402.388, C01 = 5.03711, C02 = -5.80852E-2
    REAL, PARAMETER :: C03 = 3.3420E-4, C04 = -1.47800E-6, C05 = 3.1464E-9
    REAL, PARAMETER :: C10 = 0.153563, C11 = 6.8982E-4, C12 = -8.1788E-6
    REAL, PARAMETER :: C13 = 1.3621E-7, C14 = -6.1185E-10
    REAL, PARAMETER :: C20 = 3.1260E-5, C21 = -1.7107E-6
    REAL, PARAMETER :: C22 = 2.5974E-8, C23 = -2.5335E-10, C24 = 1.0405E-12
    REAL, PARAMETER :: C30 = -9.7729E-9, C31 = 3.8504E-10, C32 = -2.3643E-12
    
! SCALE PRESSURE TO BARS
    P = P0 / 10.0
    SR = SQRT(ABS(S))
    
! S**2 TERM
    D = D0 + D1 * P
    
! S**3/2 TERM
    B1 = B10 + B11 * T
    B0 = B00 + B01 * T
    B = B0 + B1 * P
    
! S**1 TERM
    A3 = A32 * T + A31
    A3 = A3 * T + A30
    
    A2 = A23 * T + A22
    A2 = A2 * T + A21
    A2 = A2 * T + A20
    
    A1 = A14 * T + A13
    A1 = A1 * T + A12
    A1 = A1 * T + A11
    A1 = A1 * T + A10
    
    A0 = A04 * T + A03
    A0 = A0 * T + A02
    A0 = A0 * T + A01
    A0 = A0 * T + A00
    
    A = A3 * P + A2
    A = A * P + A1
    A = A * P + A0
    
! S**0 TERM
    C3 = C32 * T + C31
    C3 = C3 * T + C30
    
    C2 = C24 * T + C23
    C2 = C2 * T + C22
    C2 = C2 * T + C21
    C2 = C2 * T + C20
    
    C1 = C14 * T + C13
    C1 = C1 * T + C12
    C1 = C1 * T + C11
    C1 = C1 * T + C10
    
    C0 = C05 * T + C04
    C0 = C0 * T + C03
    C0 = C0 * T + C02
    C0 = C0 * T + C01
    C0 = C0 * T + C00
    
    C = C3 * P + C2
    C = C * P + C1
    C = C * P + C0
    
! SOUND SPEED CALCULATION
    SVEL = C + (A + B * SR + D * S) * S
    
    RETURN
END FUNCTION SVEL

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG SOUND SPEED
! ***********************************************************************
SUBROUTINE HITUNG_SVEL(S, T, P0, SVEL_RESULT)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P0
    REAL, INTENT(OUT) :: SVEL_RESULT
    REAL :: SVEL  ! Function declaration
    
    SVEL_RESULT = SVEL(S, T, P0)
    
END SUBROUTINE HITUNG_SVEL

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(S, T, P0, SVEL_RESULT)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P0, SVEL_RESULT
    
    PRINT *, ''
    PRINT *, '================= HASIL PERHITUNGAN ================='
    PRINT 100, 'SALINITY (S)         : ', S, ' PSS-78'
    PRINT 100, 'TEMPERATURE (T)      : ', T, ' DEG CELSIUS'
    PRINT 100, 'PRESSURE (P)         : ', P0, ' DECIBARS'
    PRINT *, '-----------------------------------------------------'
    PRINT 100, 'SOUND SPEED (U)      : ', SVEL_RESULT, ' METERS/SECOND'
    PRINT *, '====================================================='
    
100 FORMAT(A, F10.3, A)
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: S, T, P0, SVEL_RESULT
    INTEGER :: IOS, COUNT
    CHARACTER(50) :: FILENAME
    REAL :: SVEL  ! Function declaration
    
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
    PRINT *, '==================================================================================='
    PRINT *, ' NO | SALINITY | TEMPERATURE | PRESSURE  | SOUND SPEED |    STATUS'
    PRINT *, '    | (PSS-78) |  (DEG C)    | (DECIBARS)|   (M/S)     |'
    PRINT *, '==================================================================================='
    
    COUNT = 0
    
! LOOP MEMBACA FILE HINGGA END OF FILE
300 CONTINUE
    READ(10, *, IOSTAT=IOS) S, T, P0
    
    IF (IOS < 0) THEN
! END OF FILE
        GOTO 400
    ELSE IF (IOS > 0) THEN
! ERROR READING
        PRINT *, 'ERROR MEMBACA DATA PADA BARIS', COUNT + 1
        GOTO 300
    END IF
    
    COUNT = COUNT + 1
    SVEL_RESULT = SVEL(S, T, P0)
    
! PRINT DATA DALAM FORMAT TABEL
    PRINT 200, COUNT, S, T, P0, SVEL_RESULT, 'OK'
    
    GOTO 300
    
400 CONTINUE
    PRINT *, '==================================================================================='
    CLOSE(10)
    
    PRINT *, ''
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
200 FORMAT(I4, ' |', F8.2, '  |', F11.2, '  |', F9.1, '  |', F11.3, '  | ', A)
    
END SUBROUTINE READ_FILE

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN TABEL LENGKAP
! SESUAI DENGAN TABEL DI UNESCO TECH PAPERS NO. 44
! ***********************************************************************
SUBROUTINE TAMPIL_TABEL()
    IMPLICIT NONE
    REAL :: S, T, P, SVEL_VALUE
    INTEGER :: I, J, K
    REAL :: T_ARRAY(5), P_ARRAY(11)
    REAL :: SVEL  ! Function declaration
    INTEGER :: SALINITY
    
! ARRAY TEMPERATURE
    DATA T_ARRAY /0.0, 10.0, 20.0, 30.0, 40.0/
    
! ARRAY PRESSURE
    DATA P_ARRAY /0.0, 1000.0, 2000.0, 3000.0, 4000.0, 5000.0, &
                   6000.0, 7000.0, 8000.0, 9000.0, 10000.0/
    
    PRINT *, ''
     
! LOOP UNTUK SALINITY 25, 30, 35, 40
    DO K = 25, 40, 5
        S = REAL(K)
        SALINITY = K
        
        PRINT *, ''
        PRINT *, '============================================================================='
        PRINT *, '           SOUND SPEED IN SEAWATER U [m/s]'
        PRINT 500, '                        SALINITY: ', SALINITY
        PRINT *, '============================================================================='
        PRINT *, ''
        PRINT *, 'PRESSURE  |         TEMPERATURE (DEG C) IPTS-68'
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
                
! HITUNG SOUND SPEED
                SVEL_VALUE = SVEL(S, T, P)
                
! PRINT VALUE DENGAN FORMAT
                WRITE(*, '(F9.1)', ADVANCE='NO') SVEL_VALUE
            END DO
            
! NEW LINE
            PRINT *, ''
            
        END DO
        
        PRINT *, '----------|------------------------------------------------------------'
        
    END DO
    
    PRINT *, ''
    PRINT *, 'CHECKVALUE: SVEL = 1731.995 M/S'
    PRINT *, '            FOR S=40, T=40 DEG C, P=10000 DECIBARS'
    PRINT *, ''
    PRINT *, 'REFERENCES:'
    PRINT *, '  CHEN AND MILLERO, 1977, JASA, 62, 1129-1135'
    PRINT *, ''
    PRINT *, 'UNITS:'
    PRINT *, '  PRESSURE:    DECIBARS'
    PRINT *, '  TEMPERATURE: DEG CELSIUS (IPTS-68)'
    PRINT *, '  SALINITY:    (PSS-78)'
    PRINT *, '  SOUND SPEED: METERS/SECOND'
    PRINT *, ''
    PRINT *, 'STANDARD DEVIATION: 0.19 M/S'
    PRINT *, 'RANGE OF VALIDITY:'
    PRINT *, '  S = 0 TO 40'
    PRINT *, '  T = 0 TO 40 DEG C'
    PRINT *, '  P = 0 TO 10000 DECIBARS'
    PRINT *, ''
    
500 FORMAT(A, I2)
    
END SUBROUTINE TAMPIL_TABEL
