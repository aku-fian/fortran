! ***********************************************************************
! PROGRAM UNTUK MENGHITUNG ADIABATIC LAPSE RATE
! MENGGUNAKAN FORMULA BRYDEN (1973)
! UNESCO TECHNICAL PAPERS NO. 44 (1983)
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: S, T, P, ATG
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '======================================================'
    PRINT *, '        ADIABATIC LAPSE RATE'
    PRINT *, '          BRYDEN METHOD (1973)'
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
        CALL INPUT_MANUAL(S, T, P)
        CALL HITUNG_ATG(S, T, P, ATG)
        CALL TAMPIL_HASIL(S, T, P, ATG)
        
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
            CALL HITUNG_ATG(S, T, P, ATG)
            CALL TAMPIL_HASIL(S, T, P, ATG)
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
SUBROUTINE INPUT_MANUAL(S, T, P)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: S, T, P
    
    PRINT *, 'MASUKKAN SALINITY (S) PSS-78: '
    READ(*,*) S
    
! VALIDASI SALINITY
    IF (S < 0.0 .OR. S > 40.0) THEN
        PRINT *, 'WARNING: SALINITY DI LUAR RANGE VALID (0-40)'
    END IF
    
    PRINT *, 'MASUKKAN TEMPERATURE (T) DEG CELSIUS (IPTS-68): '
    READ(*,*) T
    
! VALIDASI TEMPERATURE
    IF (T < -2.0 .OR. T > 40.0) THEN
        PRINT *, 'WARNING: TEMPERATURE DI LUAR RANGE VALID (-2 to 40)'
    END IF
    
    PRINT *, 'MASUKKAN PRESSURE (P) DECIBARS: '
    READ(*,*) P
    
! VALIDASI PRESSURE
    IF (P < 0.0 .OR. P > 10000.0) THEN
        PRINT *, 'WARNING: PRESSURE DI LUAR RANGE VALID (0-10000)'
    END IF
    
END SUBROUTINE INPUT_MANUAL

! ***********************************************************************
! FUNCTION UNTUK MENGHITUNG ADIABATIC LAPSE RATE
! FORMULA DARI BRYDEN (1973)
! UNITS: DEG C PER DECIBAR
! ***********************************************************************
REAL FUNCTION ATG(S, T, P)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P
    REAL :: DS
    REAL :: A0, A1, A2, A3
    REAL :: B0, B1
    REAL :: C0, C1, C2, C3
    REAL :: D0, D1
    REAL :: E0, E1, E2
    
! COEFFICIENTS
    A0 = 3.5803E-5
    A1 = 8.5258E-6
    A2 = -6.8360E-8
    A3 = 6.6228E-10
    
    B0 = 1.8932E-6
    B1 = -4.2393E-8
    
    C0 = 1.8741E-8
    C1 = -6.7795E-10
    C2 = 8.7330E-12
    C3 = -5.4481E-14
    
    D0 = -1.1351E-10
    D1 = 2.7759E-12
    
    E0 = -4.6206E-13
    E1 = 1.8676E-14
    E2 = -2.1687E-16
    
! SALINITY DIFFERENCE FROM 35
    DS = S - 35.0
    
! CALCULATE ADIABATIC TEMPERATURE GRADIENT
    ATG = (((E2*T + E1)*T + E0)*P + &
          ((D1*T + D0)*DS + (((C3*T + C2)*T + C1)*T + C0))*P) * P + &
          (B1*T + B0)*DS + &
          (((A3*T + A2)*T + A1)*T + A0)
    
    RETURN
END FUNCTION ATG

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG ADIABATIC LAPSE RATE
! ***********************************************************************
SUBROUTINE HITUNG_ATG(S, T, P, ATG_RESULT)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P
    REAL, INTENT(OUT) :: ATG_RESULT
    REAL :: ATG  ! Function declaration
    
    ATG_RESULT = ATG(S, T, P)
    
END SUBROUTINE HITUNG_ATG

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(S, T, P, ATG)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P, ATG
    REAL :: ATG_PER_1000
    
! CONVERT TO DEG C PER 1000 DECIBARS FOR DISPLAY
    ATG_PER_1000 = ATG * 1000.0
    
    PRINT *, ''
    PRINT *, '================= HASIL PERHITUNGAN ================='
    PRINT 100, 'SALINITY (S)         : ', S, ' PSS-78'
    PRINT 100, 'TEMPERATURE (T)      : ', T, ' DEG CELSIUS'
    PRINT 100, 'PRESSURE (P)         : ', P, ' DECIBARS'
    PRINT 101, 'ADIABATIC LAPSE RATE : ', ATG, ' DEG C/DECIBAR'
    PRINT 100, '                       ', ATG_PER_1000, ' DEG C/1000 DECIBARS'
    PRINT *, '====================================================='
    
100 FORMAT(A, F10.3, A)
101 FORMAT(A, E14.6, A)
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: S, T, P, ATG_RESULT
    INTEGER :: IOS, COUNT
    CHARACTER(50) :: FILENAME
    REAL :: ATG  ! Function declaration
    
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
    PRINT *, '======================================================================================='
    PRINT *, ' NO | SALINITY | TEMPERATURE | PRESSURE  | ADIABATIC LAPSE RATE |    STATUS'
    PRINT *, '    | (PSS-78) | (DEG C)     | (DECIBARS)| (DEG C/1000 DBAR)    |'
    PRINT *, '======================================================================================='
    
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
    ATG_RESULT = ATG(S, T, P) * 1000.0  ! Convert to per 1000 decibars
    
! PRINT DATA DALAM FORMAT TABEL
    PRINT 200, COUNT, S, T, P, ATG_RESULT, 'OK'
    
    GOTO 300
    
400 CONTINUE
    PRINT *, '======================================================================================='
    CLOSE(10)
    
    PRINT *, ''
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
200 FORMAT(I4, ' |', F8.2, '  |', F11.2, '  |', F9.1, '  |', F18.4, '      | ', A)
    
END SUBROUTINE READ_FILE

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN TABEL LENGKAP
! SESUAI DENGAN TABEL DI UNESCO TECH PAPERS NO. 44
! ***********************************************************************
SUBROUTINE TAMPIL_TABEL()
    IMPLICIT NONE
    REAL :: S, T, P, ATG_VALUE
    INTEGER :: I, J, K
    REAL :: T_ARRAY(5), P_ARRAY(11)
    REAL :: ATG  ! Function declaration
    INTEGER :: SALINITY
    
! ARRAY TEMPERATURE
    DATA T_ARRAY /0.0, 10.0, 20.0, 30.0, 40.0/
    
! ARRAY PRESSURE
    DATA P_ARRAY /0.0, 1000.0, 2000.0, 3000.0, 4000.0, 5000.0, &
                   6000.0, 7000.0, 8000.0, 9000.0, 10000.0/
    
! LOOP UNTUK SALINITY 25, 30, 35, 40
    DO K = 25, 40, 5
        S = REAL(K)
        SALINITY = K
        
        PRINT *, ''
        PRINT *, '============================================================================='
        PRINT *, '      ADIABATIC LAPSE RATE Γ [DEG C/1000 DECIBARS]'
        PRINT 500, '                        SALINITY: ', SALINITY
        PRINT *, '============================================================================='
        PRINT *, ''
        PRINT *, 'PRESSURE  |            TEMPERATURE (DEG C) IPTS-68'
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
                
! HITUNG ADIABATIC LAPSE RATE (CONVERT TO PER 1000 DECIBARS)
                ATG_VALUE = ATG(S, T, P) * 1000.0
                
! PRINT VALUE DENGAN FORMAT
                WRITE(*, '(F9.4)', ADVANCE='NO') ATG_VALUE
            END DO
            
! NEW LINE
            PRINT *, ''
            
        END DO
        
        PRINT *, '----------|------------------------------------------------------------'
        
    END DO
    
    PRINT *, ''
    PRINT *, 'CHECKVALUE: ATG = 3.255976E-4 DEG C/DECIBAR'
    PRINT *, '                = 0.3256 DEG C/1000 DECIBARS'
    PRINT *, '            FOR S=40, T=40 DEG C, P=10000 DECIBARS'
    PRINT *, ''
    PRINT *, 'REFERENCES:'
    PRINT *, '  BRYDEN, H., 1973, DEEP-SEA RES., 20, 401-408'
    PRINT *, '  FOFONOFF, N., 1977, DEEP-SEA RES., 24, 489-491'
    PRINT *, ''
    
500 FORMAT(A, I2)
    
END SUBROUTINE TAMPIL_TABEL
