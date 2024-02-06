!=======================================================================
! Program     : Analisis Ducted Fan
! Nama		  : Deni Atmojo
! NIM		  : 13619041
! Deskripsi   : Program ini merupakan program sederhana yang digunakan
!               untuk menghitung pengaruh dari distribusi sudut serang
!               pada airfoil di ductedfan
! Pembuat     : Deni Atmojo
! Versi       : 1.0
! Tanggal     : [4 Februari 2024]
! Ini adalah file di lokal AMD Ryzen
!=======================================================================
program ADF
	! Disini perlu deskripsi variable yang digunakan
    implicit none
   
    ! Header Program Exe
    print *, '======================================================================='
    print *, 'Program     = ADF - Analisis Ducted Fan'
    print *, 'Deskripsi   = Program ini untuk menganalisis pengaruh distribusi sudut serang'
    print *, '              pada airfoil ducted fan'
    print *, 'Pembuat     = Deni Atmojo'
    Print *, 'NIM         = 13619041'
    Print *, 'Version     = 1.0'
    Print *, 'Tanggal     = [4 Februari 2024]'
    Print *, 'Pembaharuan = Belum ada pembaharuan'
    print *, '======================================================================='
    print *, ''
    print *, ''
    print *, ''
    
    do  ! Loop utama untuk program ADF
    
    ! Memanggil subroutine untuk menjalankan tugas
    call main_program
    call verifikasi
    


    end do


end program ADF

subroutine main_program
    ! Disini perlu deskripsi variable yang digunakan pada subroutine
    implicit none
    integer :: num_elements, num_blades, i, n
    real :: alpha_root, alpha_tip, step
    real, dimension(:), allocatable :: alpha_variation
    real :: CL_interp, CD_interp
    
    ! Data polar untuk NACA 4412 Re 75.000
    real, dimension(13) :: alpha_data = (/0.0, 2.0, 3.7, 5.5, 8.0, 10.0, 11.0, 12.0, 12.8, 13.5, 14.8, 15.2, 16.8/)
    real, dimension(13) :: CL_data = (/0.29, 0.425, 0.56, 0.6725, 0.82, 0.955, 1.04, 1.105, 1.15, 1.175, 1.275, 1.3, 1.35/)
    real, dimension(13) :: CD_data = (/0.21, 0.21, 0.22, 0.24, 0.245, 0.275, 0.28, 0.295, 0.31, 0.3225, 0.3225, 0.325, 0.3425/)

    
    ! Perintah untuk memberikan masukan
    write(*,'(20a)',advance='no') "Masukan banyaknya blade: "
    read*, num_blades
    write(*,'(20a)',advance='no') "Masukan banyaknya elemen simulasi: "
    read*, num_elements
    write(*,'(20a)',advance='no') "Masukan alpha_root: "
    read*, alpha_root
    write(*,'(20a)',advance='no') "Masukan alpha_tip: "
    read*, alpha_tip
    

    
    ! Menghitung selisih antara alpha root dengan alpha tip
    step = ABS(alpha_tip - alpha_root) / real(num_elements - 1)

    ! Alokasikan array
    allocate(alpha_variation(num_elements))

    ! Isi array dengan nilai yang merata
    do i = 1, num_elements
        alpha_variation(i) = alpha_root + real(i - 1) * step
    end do
    
    ! Melakukan interpolasi linier
    call interpolate_linearCLCD(alpha_data, CL_data, CD_data, n, alpha_variation, CL_interp, CD_interp)
    !call interpolate_linear(alpha_data, CD_data, n, x_interp, CL_interp)

    ! Menampilkan hasil interpolasi
    do i = 1, num_elements
    print *, "Nilai CL pada alpha = ", alpha_variation(i), " adalah ", CL_interp
    print *, "Nilai CD pada alpha = ", alpha_variation(i), " adalah ", CD_interp
    end do
    
    ! Bebaskan memori yang dialokasikan
    deallocate(alpha_variation)    


end subroutine main_program


subroutine interpolate_linearCLCD(alpha_data, CL_data, CD_data, n, alpha_variation, CL_interp, CD_interp)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: alpha_data(n), CL_data(n), CD_data(n), alpha_variation(n)  ! Perhatikan dimensi alpha_variation diubah menjadi (n)
    real, intent(out) :: CL_interp, CD_interp
    integer :: i

    ! Mencari dua titik data terdekat
    do i = 1, n-1
        if (alpha_data(i) <= alpha_variation(i) .and. alpha_data(i+1) >= alpha_variation(i)) then
            ! Melakukan interpolasi linier
            !CL_interp = CL_data(i) + (CL_data(i+1) - CL_data(i)) * (alpha_variation(i) - alpha_data(i)) / (alpha_data(i+1) - alpha_data(i))
            CL_interp = CL_data(i) + (CL_data(i+1) - CL_data(i)) * &
                  (alpha_variation(i) - alpha_data(i)) / (alpha_data(i+1) - alpha_data(i))

            !CD_interp = CD_data(i) + (CD_data(i+1) - CD_data(i)) * (alpha_variation(i) - alpha_data(i)) / (alpha_data(i+1) - alpha_data(i))
            CD_interp = CD_data(i) + (CD_data(i+1) - CD_data(i)) * &
                  (alpha_variation(i) - alpha_data(i)) / (alpha_data(i+1) - alpha_data(i))

            return
        end if
    end do
    ! Handle jika titik alpha_variation di luar jangkauan data
    print *, "Peringatan: Titik alpha yang ingin diinterpolasi berada di luar jangkauan data."
    CL_interp = 0.0
    CD_interp = 0.0
end subroutine interpolate_linearCLCD



subroutine verifikasi
    implicit none
    character :: input_char
    
    call option(input_char)
    
    ! Periksa masukan pengguna
    if (input_char == 'y' .or. input_char == 'Y') then
        ! Jika masukan adalah 'y' atau 'Y', ulangi program dari awal
        print *, ''
        print *, ''
        call main_program
    else if (input_char == 'n' .or. input_char == 'N') then
        ! Jika masukan adalah 'n' atau 'N', tutup program
        stop
    else
        ! Jika masukan tidak valid, tampilkan pesan kesalahan
        print *, ''
        print *, 'Masukan tidak valid!'
        print *, ''
        call tidak_valid
        
    end if
    
end subroutine verifikasi

subroutine option(input_char)
    ! Disini perlu deskripsi variable yang digunakan
    implicit none
    character :: input_char
    ! Tanyakan kepada pengguna apakah ingin mengulang program
    write(*,'(20a)',advance='no') "Tekan y untuk mengulang program, atau n untuk menutup :  "
    read(*, '(A)') input_char
end subroutine option

subroutine tidak_valid
    do
    call verifikasi
    end do

end subroutine tidak_valid
