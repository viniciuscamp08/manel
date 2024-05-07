program main
    implicit none
    integer :: N, S, Kmin, Kmax
    real*8 :: aa, bb
    integer :: gnf, anf, nf, mf
    real*8 :: diam_av, pear_av
    real*8, dimension(:,:), allocatable :: a
    integer, dimension(:), allocatable :: head, tail
    real*8, dimension(:), allocatable :: dis, b
    integer :: i, j, k
    character(len=100) :: filename
    logical :: valid_input

    ! Double precision aa, bb
    ! Integer N, S
    ! Integer Kmin
    ! Integer Kmax
    ! Kmin = the minimum allowed degree
    ! Kmax = the maximum allowed degree
    ! N = nodes of the structure
    ! S = # of different structures
    ! aa = the parameter "a", which measures the connectivity of the structure. aa belongs to (0, infinity).

    valid_input = .false.
    do while (.not. valid_input)
        print *, "Enter the number of nodes (integer number) N="
        read(*,*) N
        print *, N
        if (N > 0) then
            valid_input = .true.
        else
            print *, "Try again!"
        end if
    end do

    valid_input = .false.
    do while (.not. valid_input)
        print *, "Enter the number of structures S="
        read(*,*) S
        print *, S
        if (S > 0) then
            valid_input = .true.
        else
            print *, "Try again!"
        end if
    end do

    allocate(a(N, N))
    allocate(head(N-1), tail(N-1))
    allocate(dis(N), b(N))

    ! Kmin = 2
    Kmin = 2
    Kmax = N - 1

    valid_input = .false.
    do while (.not. valid_input)
        print *, "Enter the parameter a from (0,infinity): "
        read(*,*) aa
        print *, aa
        if (aa >= 0) then
            valid_input = .true.
        else
            print *, "Try again!"
        end if
    end do

    valid_input = .false.
    do while (.not. valid_input)
        print *, "Enter the parameter b from (0,infinity): "
        read(*,*) bb
        print *, bb
        if (bb >= 0) then
            valid_input = .true.
        else
            print *, "Try again!"
        end if
    end do

    gnf = int(aa * 100)
    anf = int(bb * 100)
    nf = int(real(Kmin))
    mf = int(real(Kmax))

    ! It stores the degree distribution P_k
    write(filename, "(A,'data/PkCUTOFF_N',I0,'_S',I0,'_a',I0,'_b',I0,'_Kmin',I0,'_Kmax',I0,'.dat')") N, S, gnf, anf, nf, mf
    open(unit=16, file=filename, status='replace')

    ! It stores the diameter of the network
    write(filename, "(A,'data/diamCUTOFF_N',I0,'_S',I0,'_a',I0,'_b',I0,'_Kmin',I0,'_Kmax',I0,'.dat')") N, S, gnf, anf, nf, mf
    open(unit=18, file=filename, status='replace')

    ! It stores the Pearson coefficient of the network
    write(filename, "(A,'data/pearCUTOFF_N',I0,'_S',I0,'_a',I0,'_b',I0,'_Kmin',I0,'_Kmax',I0,'.dat')") N, S, gnf, anf, nf, mf
    open(unit=19, file=filename, status='replace')

    do j = 1, N
        dis(j) = 0.0
        b(j) = 0.0
    end do

    do j = 1, N
        do i = 1, N
            a(j, i) = 0.0
        end do
    end do

    do i = 1, 50000000
        ! Generate random numbers, not sure what to do with them
        ! lin = np.random.random()
    end do

    diam_av = 0.0
    pear_av = 0.0

    do k = 1, S
        print *, "SFN", k
        ! call pair_cut(N,aa,bb,Kmin,Kmax,head,tail)
        ! call intro_matrice(N,head,tail,a)
        ! call diameter(N,a,diam)
        ! diam_av=diam_av+diam
        ! call pearson(N,a,pear)
        ! pear_av=pear_av+pear
        ! call distribution(N,a,N,b)

        dis = dis + b

    end do

    print *, "#nodes:", N, "#str:", S, "a:", aa, "b:", bb
    print *, "#K_min:", Kmin, "K_max:", Kmax
    print *, "diameter:", diam_av/real(S), "PearsonC:", pear_av/real(S)

    close(16)
    close(18)
    close(19)

end program main
