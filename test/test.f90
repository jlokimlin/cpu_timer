!*****************************************************************************80
!
!< Purpose:
!
!  Demonstrates usage of TYPE (cpu_timer_t)
!
!
!< Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!*****************************************************************************80
!
! Record of revisions:
!
!   Date             Programmer             Description of change
!   ---------      ----------------       --------------------------
!   02/06/07       John Burkardt          Original procedural code
!   2015           Jon Lo Kim Lin         Object-oriented implementation
!
!
!*****************************************************************************80
!  
program test

    use, intrinsic :: iso_fortran_env, only: &
        WP     => REAL64, &
        IP     => INT32, &
        stdout => OUTPUT_UNIT

    use type_cpu_timer_mod, only: &
        cpu_timer_t

    ! Explicit typing only
    implicit none
    
    call Test_all()
    
contains
    !
    !*****************************************************************************************
    !
    subroutine Test_all()
        !
        !---------------------------------------------------------------------------------
        ! Dictionary: local variables
        !---------------------------------------------------------------------------------
        type (cpu_timer_t)      :: timer
        !---------------------------------------------------------------------------------

        ! Print time stamp
        call timer%Print_time_stamp( )

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '*********************************************'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Demonstrates usage of TYPE (cpu_timer_t).'
        write ( stdout, '(A)' ) ' '

        call Time_random_number_routine ( )
        call Time_vectorized_EXP_routine ( )
        call Time_unvectorized_EXP_routine ( )
        call Time_2D_nearest_neighbor_problem ( )
        call Time_matrix_multiplication_problem ( )
        !
        !  Terminate.
        !
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) 'TIMER_CPU_TIME'
        write ( stdout, '(A)' ) '  Normal end of execution.'

        write ( stdout, '(A)' ) ' '

        ! Print time stamp
        call timer%Print_time_stamp( )

    end subroutine Test_all
    !
    !*****************************************************************************************
    !
    subroutine Time_random_number_routine ( )
        !
        !< Purpose:
        !
        !  Times the intrinsic RANDOM_NUMBER routine.
        !
        !
        !< Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !---------------------------------------------------------------------------------
        !
        ! Record of revisions:
        !
        !   Date             Programmer             Description of change
        !   ---------      ----------------       --------------------------
        !   05/20/09       John Burkardt          Original procedural code
        !   12/20/15       Jon Lo Kim Lin         Object-oriented implementation
        !
        !---------------------------------------------------------------------------------
        ! Dictionary: local variables
        !---------------------------------------------------------------------------------
        type (cpu_timer_t)      :: timer
        integer (IP)            :: n_log, rep   !! Counters
        integer (IP), parameter :: N_LOG_MIN = 0
        integer (IP), parameter :: N_LOG_MAX = 20
        integer (IP), parameter :: N_MIN     = 2**N_LOG_MIN
        integer (IP), parameter :: N_MAX     = 2**N_LOG_MAX
        integer (IP), parameter :: REP_NUM   = 5
        real (WP)               :: delta( N_LOG_MIN:N_LOG_MAX, REP_NUM + 3)
        real (WP)               :: x( N_MAX )
        !---------------------------------------------------------------------------------

        write ( stdout, '(A)' )     ' '
        write ( stdout, '(A)' )     '*********************************************'
        write ( stdout, '(A)' )     ' '
        write ( stdout, '(A)' )     'TIME_RANDOM_NUMBER_ROUTINE'
        write ( stdout, '(A)' )     '   time the intrinsic RANDOM_NUMBER routine:'
        write ( stdout, '(A)' )     ' '
        write ( stdout, '(A)' )     '    call random_number( x(1:n) )'
        write ( stdout, '(A)' )     ' '
        write ( stdout, '(A, I12)' ) '  Data vectors will be of minimum size ', N_MIN
        write ( stdout, '(A, I12)' ) '  Data vectors will be of maximum size ', N_MAX
        write ( stdout, '(A, I12)' ) '  Number of repetitions of the operation: ', REP_NUM

        do n_log = N_LOG_MIN, N_LOG_MAX

            do rep = 1, REP_NUM

                associate( n => 2**( n_log ) )

                    ! Start timer
                    call timer%Start()

                    call random_number( harvest = x(1:n) )

                    ! Stop timer
                    call timer%Stop()

                    ! Set CPU time
                    delta( n_log, rep ) = timer%Get_total_time()

                end associate

            end do

            delta( n_log, REP_NUM + 1) = minval ( delta( n_log,1:REP_NUM) )
            delta( n_log, REP_NUM + 2) = sum ( delta( n_log,1:REP_NUM) ) /  REP_NUM
            delta( n_log, REP_NUM + 3) = maxval ( delta( n_log,1:REP_NUM) )

        end do

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Timing results:'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '    Vector size  ' &
            //'Rep #1                Rep #2                Rep #3                ' &
            // 'Rep #4                Rep #5'
        write ( stdout, '(A)' ) ' '

        do n_log = N_LOG_MIN, N_LOG_MAX

            associate( n => 2**( n_log ) )

                write ( stdout, '(I11, 8(ES23.16))' ) n, delta( n_log,1:REP_NUM + 3)

            end associate
        end do

    end subroutine Time_random_number_routine
    !
    !*****************************************************************************************
    !
    subroutine Time_vectorized_EXP_routine( )
        !
        !< Purpose:
        !
        !  Times the vectorized EXP routine.
        !
        !
        !< Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !---------------------------------------------------------------------------------
        !
        ! Record of revisions:
        !
        !   Date             Programmer             Description of change
        !   ---------      ----------------       --------------------------
        !   02/06/07       John Burkardt          Original procedural code
        !   12/20/15       Jon Lo Kim Lin         Object-oriented implementation
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: local variables
        !--------------------------------------------------------------------------------
        type (cpu_timer_t)       :: timer
        integer (IP)             :: func, i_rep, n_log !! Counters
        integer (IP), parameter  :: N_LOG_MIN = 12
        integer (IP), parameter  :: N_LOG_MAX = 22
        integer (IP), parameter  :: N_MIN     = 2**N_LOG_MIN
        integer (IP), parameter  :: N_MAX     = 2**N_LOG_MAX
        integer (IP), parameter  :: N_REP     = 5
        real (WP),    parameter  :: PI        = acos( -1.0_WP )
        real (WP)                :: delta( N_LOG_MAX, N_REP )
        real (WP)                :: x( N_MAX )
        real (WP)                :: y( N_MAX )
        !--------------------------------------------------------------------------------

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '*********************************************'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  TIME_VECTORIZED_EXP_ROUTINE:'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '    y(1:n) =        x(1:n)  '
        write ( stdout, '(A)' ) '    y(1:n) = PI *   x(1:n)  '
        write ( stdout, '(A)' ) '    y(1:n) = sqrt ( x(1:n) )'
        write ( stdout, '(A)' ) '    y(1:n) = exp  ( x(1:n) )'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A, I12)' ) '  Data vectors will be of minimum size ', N_MIN
        write ( stdout, '(A, I12)' ) '  Data vectors will be of maximum size ', N_MAX
        write ( stdout, '(A, I12)' ) '  Number of repetitions of the operation: ', N_REP

        do func = 1, 4

            do i_rep = 1, N_REP

                do n_log = N_LOG_MIN, N_LOG_MAX

                    associate( n => 2**( n_log ) )

                        call random_number ( harvest = x(1:n) )

                        ! Start timer
                        call timer%Start()

                        ! First function
                        if ( func == 1 ) then

                            y(1:n) = x(1:n)

                        ! Second function
                        else if ( func == 2 ) then

                            y(1:n) = PI * x(1:n)

                        ! Third function
                        else if ( func == 3 ) then

                            y(1:n) = sqrt ( x(1:n) )

                        ! Fourth function
                        else if ( func == 4 ) then

                            y(1:n) = exp ( x(1:n) )

                        end if

                    end associate

                    ! Stop the timer
                    call timer%Stop()

                    ! Set CPU time
                    delta( n_log, i_rep ) = timer%Get_total_time()

                end do

            end do

            write ( stdout, '(A)' ) ' '
            write ( stdout, '(A)' ) '  Timing results:'
            write ( stdout, '(A)' ) ' '
            write ( stdout, '(A)' ) ' '
            write ( stdout, '(A)' ) '    Vector size  ' &
                //'Rep #1                Rep #2                Rep #3                ' &
                // 'Rep #4                Rep #5'
            write ( stdout, '(A)' ) ' '

            do n_log = N_LOG_MIN, N_LOG_MAX

                associate( n => 2**( n_log ) )

                    write ( stdout, '(I11, 5(ES23.16))' ) n, delta( n_log,1:N_REP)

                end associate

            end do
        end do

    end subroutine Time_vectorized_EXP_routine
    !
    !*****************************************************************************************
    !
    subroutine Time_unvectorized_EXP_routine ( )
        !
        !< Purpose:
        !
        !  Times the unvectorized EXP routine.
        !
        !
        !< Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !---------------------------------------------------------------------------------
        !
        ! Record of revisions:
        !
        !   Date             Programmer             Description of change
        !   ---------      ----------------       --------------------------
        !   02/06/07       John Burkardt          Original procedural code
        !   12/20/15       Jon Lo Kim Lin         Object-oriented implementation
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: local variables
        !--------------------------------------------------------------------------------
        type (cpu_timer_t)       :: timer
        integer (IP)             :: func, i, i_rep, n_log !! Counters
        integer (IP), parameter  :: N_LOG_MIN = 12
        integer (IP), parameter  :: N_LOG_MAX = 22
        integer (IP), parameter  :: N_MIN     = 2**N_LOG_MIN
        integer (IP), parameter  :: N_MAX     = 2**N_LOG_MAX
        integer (IP), parameter  :: N_REP     = 5
        real (WP),    parameter  :: PI        = acos( -1.0_WP )
        real (WP)                :: delta( N_LOG_MAX, N_REP )
        real (WP)                :: x(N_MAX)
        real (WP)                :: y(N_MAX)
        !--------------------------------------------------------------------------------

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '*********************************************'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) 'TIME_UNVECTORIZED_EXP_ROUTINE'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '    do i = 1, n'
        write ( stdout, '(A)' ) '      y(i) =        x(i)  '
        write ( stdout, '(A)' ) '      y(i) = PI *   x(i)  '
        write ( stdout, '(A)' ) '      y(i) = sqrt ( x(i) )'
        write ( stdout, '(A)' ) '      y(i) = exp  ( x(i) )'
        write ( stdout, '(A)' ) '    end do'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A, I12)' ) '  Data vectors will be of minimum size ',    N_MIN
        write ( stdout, '(A, I12)' ) '  Data vectors will be of maximum size ',    N_MAX
        write ( stdout, '(A, I12)' ) '  Number of repetitions of the operation: ', N_REP

        loop_over_functions: do func = 1, 4
            do i_rep = 1, N_REP
                do n_log = N_LOG_MIN, N_LOG_MAX

                    associate( n => 2**( n_log ))

                        call random_number ( harvest = x(1:n) )

                        ! Start timer
                        call timer%Start()

                        ! First function
                        if ( func == 1 ) then

                            do i = 1, n
                                y(i) = x(i)
                            end do

                        ! Second function
                        else if ( func == 2 ) then

                            do i = 1, n
                                y(i) = PI * x(i)
                            end do

                        ! Third function
                        else if ( func == 3 ) then

                            do i = 1, n
                                y(i) = sqrt ( x(i) )
                            end do

                        ! Fourth function
                        else if ( func == 4 ) then

                            do i = 1, n
                                y(i) = exp ( x(i) )
                            end do

                        end if

                    end associate

                    ! Stop timer
                    call timer%Stop()

                    ! Set CPU time
                    delta( n_log, i_rep ) = timer%Get_total_time()

                end do
            end do

            write ( stdout, '(A)' ) ' '
            write ( stdout, '(A)' ) '*********************************************'
            write ( stdout, '(A)' ) ' '
            write ( stdout, '(A)' ) '  Timing results:'
            write ( stdout, '(A)' ) ' '
            write ( stdout, '(A)' ) '    Vector size  ' &
                //'Rep #1                Rep #2                Rep #3                ' &
                // 'Rep #4                Rep #5'
            write ( stdout, '(A)' ) ' '

            do n_log = N_LOG_MIN, N_LOG_MAX

                associate( n => 2**( n_log ) )

                    write ( stdout, '(I11, 5(ES23.16))' ) n, delta( n_log,1:N_REP)

                end associate
            end do

        end do loop_over_functions

    end subroutine Time_unvectorized_EXP_routine
    !
    !*****************************************************************************************
    !
    subroutine Time_2D_nearest_neighbor_problem( )
        !
        !< Purpose:
        !
        !  Times the 2D nearest neighbor problem.
        !
        !
        !< Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !---------------------------------------------------------------------------------
        !
        ! Record of revisions:
        !
        !   Date             Programmer             Description of change
        !   ---------      ----------------       --------------------------
        !   02/06/07       John Burkardt          Original procedural code
        !   12/20/15       Jon Lo Kim Lin         Object-oriented implementation
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: local variables
        !--------------------------------------------------------------------------------
        type (cpu_timer_t)      :: timer
        integer (IP)            :: i, i_rep, n_log !! Counters
        integer (IP)            :: i_min
        integer (IP), parameter :: N_LOG_MIN = 10
        integer (IP), parameter :: N_LOG_MAX = 20
        integer (IP), parameter :: N_MIN     = 2**N_LOG_MIN
        integer (IP), parameter :: N_MAX     = 2**N_LOG_MAX
        integer (IP), parameter :: N_REP     = 5
        real (WP)               :: delta( N_LOG_MAX, N_REP )
        real (WP)               :: x( 2, N_MAX )
        real (WP)               :: y( 2 )
        real (WP)               :: dist_i, dist_min
        !--------------------------------------------------------------------------------

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '*********************************************'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  TIME_2D_NEAREST_NEIGHBOR_PROBLEM'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Given x(2,n) and y(2),'
        write ( stdout, '(A)' ) '    find x(2,*) closest to y(2).'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '    do i = 1, n'
        write ( stdout, '(A)' ) '      if distance ( x(2,i), y ) < minimum so far'
        write ( stdout, '(A)' ) '        x_min = x(2,i)'
        write ( stdout, '(A)' ) '    end do'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A, I12)' ) '  Data vectors will be of minimum size ',    N_MIN
        write ( stdout, '(A, I12)' ) '  Data vectors will be of maximum size ',    N_MAX
        write ( stdout, '(A, I12)' ) '  Number of repetitions of the operation: ', N_REP

        call random_number ( harvest = x(1:2,1:N_MAX) )
        call random_number ( harvest = y(1:2) )

        do i_rep = 1, N_REP
            do n_log = N_LOG_MIN, N_LOG_MAX

                associate( n => 2**( n_log ) )

                    ! Start timer
                    call timer%Start()

                    dist_min = huge ( dist_min )
                    i_min = 0
                    do i = 1, n

                        dist_i = sum ( ( x(1:2,i) - y(1:2) )**2 )

                        if ( dist_i < dist_min ) then

                            dist_min = dist_i
                            i_min    = i

                        end if

                    end do

                    ! Stop timer
                    call timer%Stop()

                    ! Set CPU time
                    delta( n_log, i_rep ) = timer%Get_total_time()

                end associate
            end do
        end do

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Timing results:'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '    Vector size  ' &
            //'Rep #1                Rep #2                Rep #3                ' &
            // 'Rep #4                Rep #5'
        write ( stdout, '(A)' ) ' '

        do n_log = N_LOG_MIN, N_LOG_MAX

            associate( n => 2**( n_log ) )

                write ( stdout, '(I11,5(ES23.16))' ) n, delta( n_log,1:N_REP )

            end associate

        end do

    end subroutine Time_2D_nearest_neighbor_problem
    !
    !*****************************************************************************************
    !
    subroutine Time_matrix_multiplication_problem( )
        !
        !< Purpose:
        !
        !  Times the matrix multiplication problem.
        !
        !
        !< Licensing:
        !
        !    This code is distributed under the GNU LGPL license.
        !
        !---------------------------------------------------------------------------------
        !
        ! Record of revisions:
        !
        !   Date             Programmer             Description of change
        !   ---------      ----------------       --------------------------
        !   03/05/08       John Burkardt          Original procedural code
        !   12/20/15       Jon Lo Kim Lin         Object-oriented implementation
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: local variables
        !--------------------------------------------------------------------------------
        type (cpu_timer_t)        :: timer
        integer (IP)              :: i, j, k    !! Counters
        integer (IP)              :: l_log, rep !! Counters
        integer (IP), parameter   :: L_LOG_MIN = 1
        integer (IP), parameter   :: L_LOG_MAX = 5
        integer (IP), parameter   :: L_MIN     = 4**L_LOG_MIN
        integer (IP), parameter   :: L_MAX     = 4**L_LOG_MAX
        integer (IP), parameter   :: REP_NUM   = 5
        real (WP),    allocatable :: a(:,:)
        real (WP),    allocatable :: b(:,:)
        real (WP),    allocatable :: c(:,:)
        real (WP)                 :: delta( L_LOG_MIN:L_LOG_MAX, 1:REP_NUM )
        !--------------------------------------------------------------------------------

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '*********************************************'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  TIME_MATRIX_MULTIPLICATION_PROBLEM'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Compute C = A * B'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  where'
        write ( stdout, '(A)' ) '    A is an L by M matrix,'
        write ( stdout, '(A)' ) '    B is an M by N matrix,'
        write ( stdout, '(A)' ) '  and so'
        write ( stdout, '(A)' ) '    C is an L by N matrix.'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A, I12)' ) '  Minimum value of L = M = N = ',            L_MIN
        write ( stdout, '(A, I12)' ) '  Maximum value of L = M = N = ',            L_MAX
        write ( stdout, '(A, I12)' ) '  Number of repetitions of the operation: ', REP_NUM

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Use nested DO loops for matrix multiplication.'

        do rep = 1, REP_NUM
            do l_log = L_LOG_MIN, L_LOG_MAX - 1

                associate( l => 4**( l_log ) )
                    !m = l
                    !n = l

                    allocate ( a(1:l,1:l) )
                    allocate ( b(1:l,1:l) )
                    allocate ( c(1:l,1:l) )

                    call random_number ( harvest = a(1:l,1:l) )
                    call random_number ( harvest = b(1:l,1:l) )

                    ! Start timer
                    call timer%Start()

                    do i = 1, l
                        do j = 1, l
                            c(i,j) = 0.0_WP
                            do k = 1, l
                                c(i,j) = c(i,j) + a(i,k) * b(k,j)
                            end do
                        end do
                    end do

                    ! Stop timer
                    call timer%Stop()

                    ! Get CPU time
                    delta( l_log, rep ) = timer%Get_total_time()

                    deallocate ( a )
                    deallocate ( b )
                    deallocate ( c )

                end associate
            end do
        end do

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Timing results using nested DO loops:'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '    Vector size  ' &
            //'Rep #1                Rep #2                Rep #3                ' &
            // 'Rep #4                Rep #5'
        write ( stdout, '(A)' ) ' '

        do l_log = L_LOG_MIN, L_LOG_MAX - 1
            associate( l => 4**( l_log ) )

                write ( stdout, '(I11,5(ES23.16))' ) l, delta(l_log,1:REP_NUM)

            end associate
        end do

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Use the MATMUL routine for matrix multiplication.'

        do rep = 1, REP_NUM
            do l_log = L_LOG_MIN, L_LOG_MAX

                associate ( l => 4**( l_log ) )
                    !m = l
                    !n = l

                    allocate ( a(1:l,1:l) )
                    allocate ( b(1:l,1:l) )
                    allocate ( c(1:l,1:l) )

                    call random_number ( harvest = a(1:l,1:l) )
                    call random_number ( harvest = b(1:l,1:l) )

                    ! Start timer
                    call timer%Start()

                    c = matmul ( a, b )

                    ! Stop timer
                    call timer%Stop()

                    ! Set CPU time
                    delta( l_log, rep ) = timer%Get_total_time()

                    deallocate ( a )
                    deallocate ( b )
                    deallocate ( c )

                end associate
            end do
        end do

        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '  Timing results using MATMUL:'
        write ( stdout, '(A)' ) ' '
        write ( stdout, '(A)' ) '    Vector size  ' &
            //'Rep #1                Rep #2                Rep #3                ' &
            // 'Rep #4                Rep #5'
        write ( stdout, '(A)' ) ' '

        do l_log = L_LOG_MIN, L_LOG_MAX
            associate( l => 4**( l_log ))

                write ( stdout, '(I11,5(ES23.16))' ) l, delta(l_log,1:REP_NUM)

            end associate
        end do

    end subroutine Time_matrix_multiplication_problem
    !
    !*****************************************************************************************
    !
end program test
