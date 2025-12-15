! ***********************************************************************
! PROGRAM UTAMA UNTUK KONVERSI KONDUKTIVITAS KE SALINITAS
! MENGGUNAKAN PRACTICAL SALINITY SCALE 1978 (PSS-78)
! ***********************************************************************
PROGRAM MAIN
    IMPLICIT NONE
    REAL :: CND, T, P, SAL
    INTEGER :: PILIHAN, N, I
    CHARACTER(1) :: LANJUT
    
! HEADER PROGRAM
    PRINT *, '=================================================='
    PRINT *, '   KONVERSI KONDUKTIVITAS KE SALINITAS'
    PRINT *, '   PRACTICAL SALINITY SCALE 1978 (PSS-78)'
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
        CALL INPUT_MANUAL(CND, T, P)
        CALL HITUNG_SAL(CND, T, P, SAL)
        CALL TAMPIL_HASIL(CND, T, P, SAL)
        
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
            CALL INPUT_MANUAL(CND, T, P)
            CALL HITUNG_SAL(CND, T, P, SAL)
            CALL TAMPIL_HASIL(CND, T, P, SAL)
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
SUBROUTINE INPUT_MANUAL(CND, T, P)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: CND, T, P
    
    PRINT *, 'MASUKKAN CONDUCTIVITY RATIO (CND): '
    READ(*,*) CND
    
    PRINT *, 'MASUKKAN TEMPERATURE (T) DEG CELSIUS: '
    READ(*,*) T
    
    PRINT *, 'MASUKKAN PRESSURE (P) DECIBARS: '
    READ(*,*) P
    
END SUBROUTINE INPUT_MANUAL

! ***********************************************************************
! SUBROUTINE UNTUK MENGHITUNG SALINITAS
! FORMULA DARI UNESCO TECHNICAL PAPERS IN MARINE SCIENCE NO. 44
! ***********************************************************************
SUBROUTINE HITUNG_SAL(CND, T, P, SAL)
    IMPLICIT NONE
    REAL, INTENT(IN) :: CND, T, P
    REAL, INTENT(OUT) :: SAL
    REAL :: RT, R, DT, XR
    REAL :: A0, A1, A2, A3, A4, A5
    REAL :: B0, B1, B2, B3, B4, B5
    REAL :: C0, C1, C2, C3, C4
    REAL :: D1, D2, D3, D4
    REAL :: E1, E2, E3
    REAL :: K
    REAL :: RT35, RP, DS
    REAL :: A_TERM, B_TERM, C_TERM
    
! CHECK UNTUK CONDUCTIVITY RATIO TERLALU KECIL
    IF (CND <= 5.0E-4) THEN
        SAL = 0.0
        RETURN
    END IF
    
! KONSTANTA UNTUK PERHITUNGAN SALINITAS (Equation 1)
    A0 = 0.0080
    A1 = -0.1692
    A2 = 25.3851
    A3 = 14.0941
    A4 = -7.0261
    A5 = 2.7081
    
! KONSTANTA UNTUK DELTA S (Equation 2)
    B0 = 0.0005
    B1 = -0.0056
    B2 = -0.0066
    B3 = -0.0375
    B4 = 0.0636
    B5 = -0.0144
    K = 0.0162
    
! KONSTANTA UNTUK RT35 (Equation 3)
    C0 = 0.6766097
    C1 = 2.00564E-2
    C2 = 1.104259E-4
    C3 = -6.9698E-7
    C4 = 1.0031E-9
    
! KONSTANTA UNTUK RP (Equation 4)
    E1 = 2.070E-5
    E2 = -6.370E-10
    E3 = 3.989E-15
    D1 = 3.426E-2
    D2 = 4.464E-4
    D3 = 4.215E-1
    D4 = -3.107E-3
    
    DT = T - 15.0
    
! HITUNG r_t = C(35,T,0)/C(35,15,0) - Equation 3
    RT35 = ((((C4*T + C3)*T + C2)*T + C1)*T + C0)
    
! HITUNG R_p = C(S,T,P)/C(S,T,0) - Equation 4
    C_TERM = (E3*P + E2)*P + E1
    A_TERM = D3 + D4*T
    B_TERM = 1.0 + D1*T + D2*T*T
    RP = 1.0 + (C_TERM*P) / (B_TERM + A_TERM*CND)
    
! HITUNG R_T = R/(r_t * R_p)
    R = CND
    RT = R / (RT35 * RP)
    
! XR = sqrt(R_T)
    XR = SQRT(ABS(RT))
    
! HITUNG SALINITAS - Equation 1
! S = a0 + a1*Rt^0.5 + a2*Rt + a3*Rt^1.5 + a4*Rt^2 + a5*Rt^2.5
    SAL = A0 + A1*XR + A2*RT + A3*RT*XR + A4*RT*RT + A5*RT*RT*XR
    
! KOREKSI DENGAN DELTA S - Equation 2
! ΔS = (ΔT/(1+k*ΔT)) * (b0 + b1*Rt^0.5 + b2*Rt + b3*Rt^1.5 + b4*Rt^2 + b5*Rt^2.5)
    DS = (DT / (1.0 + K*DT)) * &
         (B0 + B1*XR + B2*RT + B3*RT*XR + B4*RT*RT + B5*RT*RT*XR)
    
    SAL = SAL + DS
    
END SUBROUTINE HITUNG_SAL

! ***********************************************************************
! SUBROUTINE UNTUK MENAMPILKAN HASIL
! ***********************************************************************
SUBROUTINE TAMPIL_HASIL(CND, T, P, SAL)
    IMPLICIT NONE
    REAL, INTENT(IN) :: CND, T, P, SAL
    
    PRINT *, ''
    PRINT *, '========== HASIL PERHITUNGAN =========='
    PRINT *, 'CONDUCTIVITY RATIO : ', CND
    PRINT *, 'TEMPERATURE        : ', T, ' DEG C'
    PRINT *, 'PRESSURE           : ', P, ' DECIBARS'
    PRINT *, 'SALINITY (PSS-78)  : ', SAL
    PRINT *, '======================================='
    
END SUBROUTINE TAMPIL_HASIL

! ***********************************************************************
! SUBROUTINE UNTUK READ DATA DARI FILE
! ***********************************************************************
SUBROUTINE READ_FILE()
    IMPLICIT NONE
    REAL :: CND, T, P, SAL
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
    READ(10, *, IOSTAT=IOS) CND, T, P
    
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
    CALL HITUNG_SAL(CND, T, P, SAL)
    CALL TAMPIL_HASIL(CND, T, P, SAL)
    PRINT *, ''
    
    GOTO 300
    
400 CONTINUE
    CLOSE(10)
    
    PRINT *, 'TOTAL DATA YANG DIBACA: ', COUNT
    
END SUBROUTINE READ_FILE