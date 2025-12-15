! ***********************************************************************
! PROGRAM UNTUK MENGHITUNG FREEZING POINT TEMPERATURE OF SEAWATER
! MENGGUNAKAN FORMULA MILLERO (1976)
! UNESCO TECHNICAL PAPERS NO. 28 (1978)
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: S, P, TF
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '======================================================'
    PRINT *, '   FREEZING POINT TEMPERATURE OF SEAWATER'
    PRINT *, '         MILLERO & LEUNG METHOD (1976)'
    PRINT *, '       UNESCO TECH PAPERS NO. 28 (1978)'
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
        CALL INPUT_MANUAL(S, P)
        CALL HITUNG_TF(S, P, TF)
        CALL TAMPIL_HASIL(S, P, TF)
        
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
            CALL INPUT_MANUAL(S, P)
            CALL HITUNG_TF(S, P, TF)
            CALL TAMPIL_HASIL(S, P, TF)
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
SUBROUTINE INPUT_MANUAL(S, P)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: S, P
    
    PRINT *, 'MASUKKAN SALINITY (S) PSS-78: '
    READ(*,*) S
    
! VALIDASI SALINITY
    IF (S < 4.0 .OR. S > 40.0) THEN
        PRINT *, 'WARNING: SALINITY DI LUAR RANGE VALID (4-40)'
    END IF
    
    PRINT *, 'MASUKKAN PRESSURE (P) DECIBARS: '
    READ(*,*) P
    
! VALIDASI PRESSURE
    IF (P < 0.0 .OR. P > 500.0) THEN
        PRINT *, 'WARNING: PRESSURE DI LUAR RANGE VALID (0-500)'
    END IF
    
END SUBROUTINE INPUT_MANUAL

! ***********************************************************************
! FUNCTION UNTUK MENGHITUNG FREEZING POINT TEMPERATURE
! FORMULA: TF = (A0 + A1*SQRT(S) + A2*S)*S + B*P
! ***********************************************************************
REAL FUNCTION TF(S, P)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, P
    REAL :: A0, A1, A2, B
    
! KONSTANTA FORMULA
    A0 = -0.0575
    A1 = 1.710523E-3
    A2 = -2.154996E-4
    B = -7.53E-4
    
! HITUNG FREEZING POINT
    TF = (A0 + A1*SQRT(ABS(S)) + A2*S) * S + B*P
    
    RETURN
END FUNCTION TF

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG FREEZING POINT
! ***********************************************************************
SUBROUTINE HITUNG_TF(S, P, TF_RESULT)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, P
    REAL, INTENT(OUT) :: TF_RESULT
    REAL :: TF  ! Function declaration
    
    TF_RESULT = TF(S, P)
    
END SUBROUTINE HITUNG_TF

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(S, P, TF)
    IMPLICIT NONE
    REAL, INTENT(IN) :: S, P, TF
    
    PRINT *, ''
    PRINT *, '============= HASIL PERHITUNGAN ============='
    PRINT 100, 'SALINITY (S)    : ', S, ' PSS-78'
    PRINT 100, 'PRESSURE (P)    : ', P, ' DECIBARS'
    PRINT 100, 'FREEZING POINT  : ', TF, ' DEG CELSIUS'
    PRINT *, '============================================='
    
100 FORMAT(A, F10.3, A)
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: S, P, TF_RESULT
    INTEGER :: IOS, COUNT
    CHARACTER(50) :: FILENAME
    REAL :: TF  ! Function declaration
    
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
    PRINT *, '================================================================'
    PRINT *, ' NO |  SALINITY  | PRESSURE  | FREEZING POINT |    STATUS'
    PRINT *, '    |  (PSS-78)  | (DECIBARS)| (DEG CELSIUS)  |'
    PRINT *, '================================================================'
    
    COUNT = 0
    
! LOOP MEMBACA FILE HINGGA END OF FILE
300 CONTINUE
    READ(10, *, IOSTAT=IOS) S, P
    
    IF (IOS < 0) THEN
! END OF FILE
        GOTO 400
    ELSE IF (IOS > 0) THEN
! ERROR READING
        PRINT *, 'ERROR MEMBACA DATA PADA BARIS', COUNT + 1
        GOTO 300
    END IF
    
    COUNT = COUNT + 1
    TF_RESULT = TF(S, P)
    
! PRINT DATA DALAM FORMAT TABEL
    PRINT 200, COUNT, S, P, TF_RESULT, 'OK'
    
    GOTO 300
    
400 CONTINUE
    PRINT *, '================================================================'
    CLOSE(10)
    
    PRINT *, ''
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
200 FORMAT(I4, ' |', F10.3, '  |', F9.3, '  |', F13.6, '   | ', A)
    
END SUBROUTINE READ_FILE

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN TABEL LENGKAP
! SESUAI DENGAN TABEL DI UNESCO TECH PAPERS NO. 28
! ***********************************************************************
SUBROUTINE TAMPIL_TABEL()
    IMPLICIT NONE
    REAL :: S, P, TF_VALUE
    INTEGER :: I, J
    REAL :: S_ARRAY(8), P_ARRAY(6)
    REAL :: TF  ! Function declaration
    
! ARRAY SALINITY
    DATA S_ARRAY /5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0/
    
! ARRAY PRESSURE
    DATA P_ARRAY /0.0, 100.0, 200.0, 300.0, 400.0, 500.0/
    
    PRINT *, ''
    PRINT *, '========================================================================='
    PRINT *, '           FREEZING POINT TEMPERATURE OF SEAWATER [DEG C]'
    PRINT *, '========================================================================='
    PRINT *, ''
    PRINT *, 'PRESSURE |                    SALINITY (PSS-78)'
    PRINT *, 'DECIBARS |    5      10      15      20      25      30      35      40'
    PRINT *, '---------|----------------------------------------------------------------'
    
! LOOP UNTUK SETIAP PRESSURE
    DO J = 1, 6
        P = P_ARRAY(J)
        
! PRINT PRESSURE VALUE
        WRITE(*, '(F8.0, A)', ADVANCE='NO') P, '  |'
        
! LOOP UNTUK SETIAP SALINITY
        DO I = 1, 8
            S = S_ARRAY(I)
            
! HITUNG FREEZING POINT
            TF_VALUE = TF(S, P)
            
! PRINT VALUE DENGAN FORMAT
            WRITE(*, '(F7.3)', ADVANCE='NO') TF_VALUE
        END DO
        
! NEW LINE
        PRINT *, ''
        
    END DO
    
    PRINT *, '---------|----------------------------------------------------------------'
    PRINT *, ''
    PRINT *, 'CHECKVALUE: TF = -2.588567 DEG C FOR S=40.0, P=500.0 DECIBARS'
    PRINT *, 'VALID RANGE: SALINITY 4-40 (PSS-78), PRESSURE 0-500 DECIBARS'
    PRINT *, ''
    
END SUBROUTINE TAMPIL_TABEL
