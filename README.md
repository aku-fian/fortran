Fortran Algorithms for Seawater Properties
Algorithms for Computation of Fundamental Properties of Seawater
Repositori ini berisi implementasi kode sumber Fortran untuk sembilan (9) algoritma yang digunakan dalam komputasi properti mendasar air laut. Algoritma-algoritma ini didukung oleh Unesco/SCOR/ICES/IAPSO Joint Panel on Oceanographic Tables and Standards dan dipublikasikan dalam:
Dokumen Sumber (The Paper):
Unesco technical papers in marine science 44: Algorithms for computation of fundamental properties of seawater (Unesco, 1983).
Akses Paper di sini: paper pdok.pdf

Cara Mengkompilasi Ulang Kode Fortran
Jika Anda berada di sistem non-Windows atau ingin memastikan kode berjalan dengan kompiler terbaru, Anda dapat mengkompilasi ulang file sumber .f.
Persyaratan:
Kompiler Fortran yang mendukung standar Fortran 77 atau Fortran 90/95 (misalnya, gfortran, Intel Fortran, atau Flang).

Langkah-Langkah Menggunakan GFortran (Rekomendasi)
1. Pastikan gfortran terinstal di sistem Anda.
2. Buka Terminal/Command Prompt dan navigasikan ke direktori ini.
3. Untuk mengkompilasi, jalankan perintah berikut (contoh menggunakan density.f):
Bash:
gfortran density.f -o density_new.exe
#-o density_new.exe menentukan nama executable output.
4.Jalankan file yang baru dikompilasi:
Bash:
./density_new.exe
#(Tergantung bagaimana skrip Fortran Anda diatur, mungkin memerlukan input parameter setelah dijalankan).
