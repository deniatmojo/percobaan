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
    call linspace
    
    end do


end program ADF

subroutine linspace
    ! Disini perlu deskripsi variable yang digunakan pada subroutine
    implicit none
    integer :: num_elements, num_blades, i
    real :: alpha_root, alpha_tip, step
    real, dimension(:), allocatable :: alpha_variation
        
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

    ! Cetak hasil
    print *, "Array alpha_variation:"
    do i = 1, num_elements
        print *, alpha_variation(i)
    end do

    ! Bebaskan memori yang dialokasikan
    deallocate(alpha_variation)
    
    do
    
    call verifikasi
    
    end do

end subroutine linspace

subroutine verifikasi
    implicit none
    character :: input_char
    
    

    call option(input_char)
    
    ! Periksa masukan pengguna
    if (input_char == 'y' .or. input_char == 'Y') then
        ! Jika masukan adalah 'y' atau 'Y', ulangi program dari awal
        print *, ''
        print *, ''
        call linspace
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
