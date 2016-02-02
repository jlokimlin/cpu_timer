!*****************************************************************************************
!
!< Purpose:
!
! Defines a class for timing program execution
!
module type_CpuTimer

    use, intrinsic :: iso_fortran_env, only: &
        WP     => REAL64, &
        IP     => INT32, &
        stdout => OUTPUT_UNIT

    ! Explicit typing only
    implicit none

    ! Everything is private unless stated otherwise
    private
    public :: CpuTimer

    !---------------------------------------------------------------------------------
    ! Dictionary: global variables confined to the module
    !---------------------------------------------------------------------------------
    integer (IP), parameter  :: REQUEST_TIME_IN_SECONDS = 0_IP
    integer (IP), parameter  :: REQUEST_TIME_IN_MINUTES = 1_IP
    integer (IP), parameter  :: REQUEST_TIME_IN_HOURS   = 2_IP
    !---------------------------------------------------------------------------------

    ! Declare derived data type
    type, public :: CpuTimer

        ! All components are private unless stated otherwise
        private

        !---------------------------------------------------------------------------------
        ! Timer status flags
        !---------------------------------------------------------------------------------
        logical         :: initialized     = .false.
        logical         :: timer_started   = .false.
        logical         :: timer_stopped   = .false.
        !---------------------------------------------------------------------------------
        real (WP)       :: cpu_start_time  = 0.0_WP
        real (WP)       :: cpu_finish_time = 0.0_WP
        !---------------------------------------------------------------------------------
        integer (IP)    :: initial_ticks   = 0_IP
        integer (IP)    :: final_ticks     = 0_IP  ! final value of the clock tick counter
        integer (IP)    :: count_max       = 0_IP  ! maximum value of the clock counter
        integer (IP)    :: count_rate      = 0_IP  ! number of clock ticks per second
        integer (IP)    :: num_ticks       = 0_IP  ! number of clock ticks of the code
        !---------------------------------------------------------------------------------

    contains

        ! All methods are private unless stated otherwise
        private

        !---------------------------------------------------------------------------------
        ! Public methods
        !---------------------------------------------------------------------------------
        procedure, public         :: start => start_timer
        procedure, public         :: stop  => stop_timer
        procedure, public         :: get_total_time
        procedure, public         :: get_elapsed_time
        procedure, public, nopass :: print_time_stamp
        !---------------------------------------------------------------------------------
        ! Private methods
        !---------------------------------------------------------------------------------
        procedure                 :: create  => initialize_timer
        procedure                 :: destroy => destruct_timer
        !---------------------------------------------------------------------------------
        ! Finalizer
        !---------------------------------------------------------------------------------
        final                     :: finalize_CpuTimer
        !---------------------------------------------------------------------------------

    end type CpuTimer

contains
    !
    !*****************************************************************************************
    !
    ! Public methods
    !
    !*****************************************************************************************
    !
    subroutine start_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out) :: this
        !--------------------------------------------------------------------------------

        ! Initialize timer
        call this%Create()

        ! Set CPU start time
        call cpu_time( this%cpu_start_time )

        ! Set initial ticks
        call system_clock( count = this%initial_ticks )

        ! Set timer status
        this%timer_started = .true.

    end subroutine start_timer
    !
    !*****************************************************************************************
    !
    subroutine stop_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out) :: this
        !--------------------------------------------------------------------------------

        ! Check timer flag
        if ( .not. this%timer_started ) return

        ! Set cpu finihs time
        call cpu_time( this%cpu_finish_time )

        ! Set final ticks
        call system_clock( count = this%final_ticks )

        ! Set timer status
        this%timer_stopped = .true.

    end subroutine stop_timer
    !
    !*****************************************************************************************
    !
    function get_total_time( this, units ) result( return_value )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out)        :: this
        integer (IP),        intent (in), optional  :: units
        real (WP)                                   :: return_value
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Return zero if the timer was never started
        !--------------------------------------------------------------------------------

        if ( .not.this%timer_started )     then

            return_value = 0.0_WP
            return

        end if

        !--------------------------------------------------------------------------------
        ! If the timer was not stopped, then return the current time elapsed
        !--------------------------------------------------------------------------------

        if ( .not.this%timer_stopped )     then

            call this%stop()

        end if

        !--------------------------------------------------------------------------------
        ! Set total time
        !--------------------------------------------------------------------------------

        associate( &
            start  => this%cpu_start_time, &
            finish => this%cpu_finish_time &
            )

            return_value =  finish - start

        end associate

        !--------------------------------------------------------------------------------
        ! Convert to requested units if present
        !--------------------------------------------------------------------------------

        if ( present( units ) )  then

            select case ( units )
                case( REQUEST_TIME_IN_MINUTES )

                    return_value = return_value / 60

                case( REQUEST_TIME_IN_HOURS )

                    return_value = return_value / 3600

                case default

            end select
        end if

    end function get_total_time
    !
    !*****************************************************************************************
    !
    function get_elapsed_time( this, units ) result ( return_value )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out)        :: this
        integer (IP),        intent (in), optional  :: units
        real (WP)                                   :: return_value
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Return zero if the timer was never started
        !--------------------------------------------------------------------------------

        if ( .not.this%timer_started )     then

            return_value = 0.0_WP
            return

        end if

        !--------------------------------------------------------------------------------
        ! If the timer was not stopped, then return the current time elapsed
        !--------------------------------------------------------------------------------

        if ( .not.this%timer_stopped )     then

            call this%stop()

        end if

        !--------------------------------------------------------------------------------
        ! Set elapsed time in seconds
        !--------------------------------------------------------------------------------
        associate( &
            num        => this%num_ticks, &
            final      => this%final_ticks, &
            initial    => this%initial_ticks, &
            count_max  => this%count_max, &
            count_rate => this%count_rate &
            )

            num = final - initial

            if ( final < initial ) then

                num = num + count_max

            end if

            return_value = real( num, WP) / count_rate

        end associate

        !--------------------------------------------------------------------------------
        ! Convert to requested units if present
        !--------------------------------------------------------------------------------

        if ( present( units ) ) then

            select case ( units )

                case( REQUEST_TIME_IN_MINUTES )

                    return_value = return_value/ 60

                case( REQUEST_TIME_IN_HOURS )

                    return_value = return_value/ 3600

                case default

            end select

        end if

    end function get_elapsed_time
    !
    !*****************************************************************************************
    !
    subroutine print_time_stamp( file_unit )
        !
        !< Purpose:
        !
        !  prints the current YMDHMS date as a time stamp.
        !
        !< Example:
        !
        !  May 31 2001   9:45:54.872 AM
        !
        !< Licensing:
        !
        !  This code is distributed under the GNU LGPL license.
        !
        !---------------------------------------------------------------------------------
        !
        ! Record of revisions:
        !
        !   Date             Programmer             Description of change
        !   ---------      ----------------       --------------------------
        !   05/31/01       John Burkardt          Original procedural code
        !   12/20/15       Jon Lo Kim Lin         Object-oriented implementation
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        integer (IP), intent (in), optional :: file_unit
        !--------------------------------------------------------------------------------
        ! Dictionary: local variables
        !--------------------------------------------------------------------------------
        integer (IP)                  :: values(8)
        character (len=10)            :: time
        character (len=5)             :: zone
        character (len=8)             :: am_or_pm
        character (len=8)             :: date
        character (len=9), parameter  :: LIST_OF_MONTHS(*) = [ &
            'January  ', 'February ', 'March    ', 'April    ', &
            'May      ', 'June     ', 'July     ', 'August   ', &
            'September', 'October  ', 'November ', 'December ' ]
        character (len=*), parameter  :: TIME_FORMAT = &
            '( A, 1X, I2, 1X, I4, 2X, I2, A1, I2.2, A1, I2.2, A1, I3.3, 1X, A)'
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Get the corresponding date and time information from the real-time system clock
        !--------------------------------------------------------------------------------

        call date_and_time( date, time, zone, values )

        !--------------------------------------------------------------------------------
        ! Associate results
        !--------------------------------------------------------------------------------

        associate( &
            year        => values(1), &
            month       => values(2), &
            day         => values(3), &
            hour        => values(5), &
            minute      => values(6), &
            second      => values(7), &
            millisecond => values(8) &
            )

            if ( hour < 12 ) then
                am_or_pm = 'AM'
            else if ( hour == 12 ) then
                if ( minute == 0 .and. second == 0 ) then
                    am_or_pm = 'Noon'
                else
                    am_or_pm = 'PM'
                end if
            else
                hour = hour - 12
                if ( hour < 12 ) then
                    am_or_pm = 'PM'
                else if ( hour == 12 ) then
                    if ( minute == 0 .and. second == 0 ) then
                        am_or_pm = 'Midnight'
                    else
                        am_or_pm = 'AM'
                    end if
                end if
            end if

            !--------------------------------------------------------------------------------
            ! Return time stamp
            !--------------------------------------------------------------------------------

            if ( present( file_unit )) then

                write ( file_unit, fmt =  TIME_FORMAT ) &
                    trim( LIST_OF_MONTHS( month ) ), &
                    day, year, hour, ':', &
                    minute, ':', second, '.', &
                    millisecond, trim( am_or_pm )

            else

                write ( stdout, fmt = TIME_FORMAT ) &
                    trim( LIST_OF_MONTHS( month ) ), &
                    day, year, hour, ':', &
                    minute, ':', second, '.', &
                    millisecond, trim( am_or_pm )

            end if

        end associate

    end subroutine print_time_stamp
    !
    !*****************************************************************************************
    !
    ! Private methods
    !
    !*****************************************************************************************
    !
    subroutine initialize_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out) :: this
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Check initialization flag
        !--------------------------------------------------------------------------------

        if ( this%initialized ) then

            call this%Destroy()

        end if


        !--------------------------------------------------------------------------------
        ! Initialize counters
        !--------------------------------------------------------------------------------

        call system_clock (&
            count_rate = this%count_rate, &
            count_max  = this%count_max )

    end subroutine initialize_timer
    !
    !*****************************************************************************************
    !
    subroutine destruct_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out) :: this
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Check status
        !--------------------------------------------------------------------------------

        if ( .not. this%initialized ) return

        !---------------------------------------------------------------------------------
        ! Reset booleans
        !---------------------------------------------------------------------------------

        this%initialized     = .false.
        this%timer_started   = .false.
        this%timer_stopped   = .false.

        !---------------------------------------------------------------------------------
        ! Reset floats
        !---------------------------------------------------------------------------------

        this%cpu_start_time  = 0.0_WP
        this%cpu_finish_time = 0.0_WP

        !---------------------------------------------------------------------------------
        ! Reset integers
        !---------------------------------------------------------------------------------

        this%initial_ticks   = 0_IP
        this%final_ticks     = 0_IP
        this%count_max       = 0_IP
        this%count_rate      = 0_IP
        this%num_ticks       = 0_IP

    end subroutine destruct_timer
    !
    !*****************************************************************************************
    !
    subroutine finalize_CpuTimer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        type (CpuTimer), intent (in out) :: this
        !--------------------------------------------------------------------------------

        call this%Destroy()

    end subroutine finalize_CpuTimer
    !
    !*****************************************************************************************
    !
end module type_CpuTimer
