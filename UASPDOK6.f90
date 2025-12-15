! ***********************************************************************
! PROGRAM UNTUK MENGHITUNG SPECIFIC HEAT OF SEAWATER
! MENGGUNAKAN FORMULA MILLERO ET AL (1973)
! UNESCO TECHNICAL PAPERS NO. 38 (1981)
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: S, T, P, CPSW
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '======================================================'
    PRINT *, '      SPECIFIC HEAT OF SEAWATER'
    PRINT *, '        MILLERO ET AL METHOD (1973)'
    PRINT *, '       UNESCO TECH PAPERS NO. 38 (1981)'
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
        CALL HITUNG_CPSW(S, T, P, CPSW)
        CALL TAMPIL_HASIL(S, T, P, CPSW)
        
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
            CALL HITUNG_CPSW(S, T, P, CPSW)
            CALL TAMPIL_HASIL(S, T, P, CPSW)
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
    IF (T < 0.0 .OR. T > 40.0) THEN
        PRINT *, 'WARNING: TEMPERATURE DI LUAR RANGE VALID (0-40)'
    END IF
    
    PRINT *, 'MASUKKAN PRESSURE (P) DECIBARS: '
    READ(*,*) P
    
! VALIDASI PRESSURE
    IF (P < 0.0 .OR. P > 10000.0) THEN
        PRINT *, 'WARNING: PRESSURE DI LUAR RANGE VALID (0-10000)'
    END IF
    
END SUBROUTINE INPUT_MANUAL

! ***********************************************************************
! FUNCTION UNTUK MENGHITUNG SPECIFIC HEAT OF SEAWATER
! FORMULA DARI MILLERO ET AL (1973) + PRESSURE CORRECTION
! ***********************************************************************
REAL FUNCTION CPSW(S, T, P0)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P0
    REAL :: P, SR, A, B, C, CPO, CPI, CP2
    REAL :: C0, C1, C2, C3, C4
    REAL :: A0, A1, A2, B0, B1, B2
    REAL :: D0, D1, D2, D3, D4
    REAL :: E0, E1, E2
    REAL :: F0, F1, F2, F3, G0
    REAL :: H0, H1, H2, J1
    
! SCALE PRESSURE TO BARS
    P = P0 / 10.0
    
! SQRT SALINITY FOR FRACTIONAL TERMS
    SR = SQRT(ABS(S))
    
! ===================================================================
! SPECIFIC HEAT CPO FOR P=0 (MILLERO ET AL, UNESCO REPORT NO. 38 1981)
! ===================================================================
    
! COEFFICIENTS FOR CPO
    C0 = 4217.4
    C1 = -3.720283
    C2 = 0.1412855
    C3 = -2.654387E-3
    C4 = 2.093236E-5
    
    A0 = -7.643575
    A1 = 0.1072763
    A2 = -1.38385E-3
    
    B0 = 0.1770383
    B1 = -4.07718E-3
    B2 = 5.148E-5
    
    A = (A2*T + A1)*T + A0
    B = (B2*T + B1)*T + B0
    C = (((C4*T + C3)*T + C2)*T + C1)*T + C0
    
    CPO = (B*SR + A)*S + C
    
! ===================================================================
! CPI PRESSURE AND TEMPERATURE TERMS FOR S = 0
! ===================================================================
    
    A = ((((1.7168E-8*T + 2.0357E-6)*T - 3.13885E-4)*T + &
         1.45747E-2)*T - 0.49592)
    B = ((((2.2956E-11*T - 4.0027E-9)*T + 2.87533E-7)*T - &
         1.08645E-5)*T + 2.4931E-4)
    C = ((6.136E-13*T - 6.5637E-11)*T + 2.6380E-9)*T - 5.422E-8
    
    CPI = ((C*P + B)*P + A)*P
    
! ===================================================================
! CP2 PRESSURE AND TEMPERATURE TERMS FOR S > 0
! ===================================================================
    
    D0 = 4.9247E-3
    D1 = -1.28315E-4
    D2 = 9.802E-7
    D3 = 2.5941E-8
    D4 = -2.9179E-10
    
    E0 = -1.2331E-4
    E1 = -1.517E-6
    E2 = 3.122E-8
    
    F0 = -2.9558E-6
    F1 = 1.17054E-7
    F2 = -2.3905E-9
    F3 = 1.8448E-11
    
    G0 = 9.971E-8
    
    H0 = 5.540E-10
    H1 = -1.7682E-11
    H2 = 3.513E-13
    
    J1 = -1.4300E-12
    
    A = ((((D4*T + D3)*T + D2)*T + D1)*T + D0)
    B = (E2*T + E1)*T + E0
    A = (A + B*SR)*S
    
    B = (((F3*T + F2)*T + F1)*T + F0)
    B = (B + G0*SR)*S
    
    C = (H2*T + H1)*T + H0
    C = (C + J1*T*SR)*S
    
    CP2 = ((C*P + B)*P + A)*P
    
! SPECIFIC HEAT RETURN
    CPSW = CPO + CPI + CP2
    
    RETURN
END FUNCTION CPSW

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG SPECIFIC HEAT
! ***********************************************************************
SUBROUTINE HITUNG_CPSW(S, T, P, CPSW_RESULT)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P
    REAL, INTENT(OUT) :: CPSW_RESULT
    REAL :: CPSW  ! Function declaration
    
    CPSW_RESULT = CPSW(S, T, P)
    
END SUBROUTINE HITUNG_CPSW

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(S, T, P, CPSW)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, T, P, CPSW
    
    PRINT *, ''
    PRINT *, '=============== HASIL PERHITUNGAN ==============='
    PRINT 100, 'SALINITY (S)    : ', S, ' PSS-78'
    PRINT 100, 'TEMPERATURE (T) : ', T, ' DEG CELSIUS'
    PRINT 100, 'PRESSURE (P)    : ', P, ' DECIBARS'
    PRINT 100, 'SPECIFIC HEAT   : ', CPSW, ' J/(KG DEG C)'
    PRINT *, '================================================='
    
100 FORMAT(A, F10.3, A)
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: S, T, P, CPSW_RESULT
    INTEGER :: IOS, COUNT
    CHARACTER(50) :: FILENAME
    REAL :: CPSW  ! Function declaration
    
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
    PRINT *, ' NO | SALINITY | TEMPERATURE | PRESSURE  |  SPECIFIC HEAT  |    STATUS'
    PRINT *, '    | (PSS-78) | (DEG C)     | (DECIBARS)| (J/(KG DEG C))  |'
    PRINT *, '==================================================================================='
    
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
    CPSW_RESULT = CPSW(S, T, P)
    
! PRINT DATA DALAM FORMAT TABEL
    PRINT 200, COUNT, S, T, P, CPSW_RESULT, 'OK'
    
    GOTO 300
    
400 CONTINUE
    PRINT *, '==================================================================================='
    CLOSE(10)
    
    PRINT *, ''
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
200 FORMAT(I4, ' |', F8.2, '  |', F11.2, '  |', F9.1, '  |', F14.3, '   | ', A)
    
END SUBROUTINE READ_FILE

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN TABEL LENGKAP
! SESUAI DENGAN TABEL DI UNESCO TECH PAPERS NO. 38
! ***********************************************************************
SUBROUTINE TAMPIL_TABEL()
    IMPLICIT NONE
    REAL :: S, T, P, CPSW_VALUE
    INTEGER :: I, J, K
    REAL :: T_ARRAY(5), P_ARRAY(11)
    REAL :: CPSW  ! Function declaration
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
        PRINT *, '         SPECIFIC HEAT OF SEAWATER Cp [J/(KG DEG C)]'
        PRINT 500, '                        SALINITY: ', SALINITY
        PRINT *, '============================================================================='
        PRINT *, ''
        PRINT *, 'PRESSURE  |              TEMPERATURE (DEG C) IPTS-68'
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
                
! HITUNG SPECIFIC HEAT
                CPSW_VALUE = CPSW(S, T, P)
                
! PRINT VALUE DENGAN FORMAT
                WRITE(*, '(F9.1)', ADVANCE='NO') CPSW_VALUE
            END DO
            
! NEW LINE
            PRINT *, ''
            
        END DO
        
        PRINT *, '----------|------------------------------------------------------------'
        
    END DO
    
    PRINT *, ''
    PRINT *, 'CHECKVALUE: CPSW = 3849.500 J/(KG DEG C)'
    PRINT *, '            FOR S=40, T=40 DEG C, P=10000 DECIBARS'
    PRINT *, ''
    PRINT *, 'VALID RANGE: S=0-40 (PSS-78), T=0-40 DEG C, P=0-10000 DECIBARS'
    PRINT *, ''
    
500 FORMAT(A, I2)
    
END SUBROUTINE TAMPIL_TABEL