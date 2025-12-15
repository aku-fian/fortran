! ***********************************************************************
! PROGRAM UNTUK KONVERSI PRESSURE KE DEPTH
! MENGGUNAKAN METODE SAUNDERS DAN FOFONOFF (1976)
! DENGAN FORMULA YANG TELAH DI-REFIT UNTUK EOS-80
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: P, LAT, DEPTH
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '=================================================='
    PRINT *, '      KONVERSI PRESSURE KE DEPTH'
    PRINT *, '   SAUNDERS & FOFONOFF METHOD (1976)'
    PRINT *, '      REFITTED FOR EOS-80 (1983)'
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
        CALL INPUT_MANUAL(P, LAT)
        CALL HITUNG_DEPTH(P, LAT, DEPTH)
        CALL TAMPIL_HASIL(P, LAT, DEPTH)
        
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
            CALL INPUT_MANUAL(P, LAT)
            CALL HITUNG_DEPTH(P, LAT, DEPTH)
            CALL TAMPIL_HASIL(P, LAT, DEPTH)
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
SUBROUTINE INPUT_MANUAL(P, LAT)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: P, LAT
    
    PRINT *, 'MASUKKAN PRESSURE (P) DECIBARS: '
    READ(*,*) P
    
    PRINT *, 'MASUKKAN LATITUDE (LAT) DEGREES: '
    READ(*,*) LAT
    
END SUBROUTINE INPUT_MANUAL

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG DEPTH DARI PRESSURE
! FORMULA DARI UNESCO TECHNICAL PAPERS NO. 44 (1983)
! DEPTH IN METERS FROM PRESSURE IN DECIBARS
! ***********************************************************************
SUBROUTINE HITUNG_DEPTH(P, LAT, DEPTH)
    IMPLICIT NONE
    REAL, INTENT(IN) :: P, LAT
    REAL, INTENT(OUT) :: DEPTH
    REAL :: X, GR
    REAL :: C1, C2, C3, C4
    
! KONSTANTA UNTUK PRESSURE POLYNOMIAL
! Formula: Depth = (C1*P + C2*P^2 + C3*P^3 + C4*P^4) / g(lat)
    C1 = 9.72659
    C2 = -2.2512E-5
    C3 = 2.279E-10
    C4 = -1.82E-15
    
! HITUNG X = SIN^2(LATITUDE)
    X = SIN(LAT / 57.29578)  ! Konversi degree ke radian
    X = X * X
    
! HITUNG GRAVITY VARIATION WITH LATITUDE
! GR = GRAVITY VARIATION WITH LATITUDE: ANON (1970) BULLETIN GEODESIQUE
! Formula: g(lat) = 9.780318 * (1 + (5.2788E-3 + 2.36E-5*X)*X) + 1.092E-6*P
    GR = 9.780318 * (1.0 + (5.2788E-3 + 2.36E-5*X)*X) + 1.092E-6*P
    
! HITUNG DEPTH MENGGUNAKAN POLYNOMIAL
! Depth = (C1*P + C2*P^2 + C3*P^3 + C4*P^4) / GR
    DEPTH = (((C4*P + C3)*P + C2)*P + C1) * P
    DEPTH = DEPTH / GR
    
END SUBROUTINE HITUNG_DEPTH

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(P, LAT, DEPTH)
    IMPLICIT NONE
    REAL, INTENT(IN) :: P, LAT, DEPTH
    
    PRINT *, ''
    PRINT *, '========== HASIL PERHITUNGAN =========='
    PRINT *, 'PRESSURE (P)    : ', P, ' DECIBARS'
    PRINT *, 'LATITUDE        : ', LAT, ' DEGREES'
    PRINT *, 'DEPTH           : ', DEPTH, ' METERS'
    PRINT *, '======================================='
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: P, LAT, DEPTH
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
    READ(10, *, IOSTAT=IOS) P, LAT
    
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
    CALL HITUNG_DEPTH(P, LAT, DEPTH)
    CALL TAMPIL_HASIL(P, LAT, DEPTH)
    PRINT *, ''
    
    GOTO 300
    
400 CONTINUE
    CLOSE(10)
    
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
END SUBROUTINE READ_FILE