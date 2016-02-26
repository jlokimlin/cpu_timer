!*****************************************************************************************
!
!< Purpose:
!
! Defines a class for timing program execution
!
module type_CpuTimer

    use, intrinsic :: iso_fortran_env, only: &
        wp     => REAL64, &
        ip     => INT32, &
        stdout => OUTPUT_UNIT, &
        stderr => ERROR_UNIT

    ! Explicit typing only
    implicit none

    ! Everything is private unless stated otherwise
    private
    public :: CpuTimer

    !---------------------------------------------------------------------------------
    ! Dictionary: global variables confined to the module
    !---------------------------------------------------------------------------------
    integer (ip), parameter :: REQUEST_TIME_IN_SECONDS = 0_ip
    integer (ip), parameter :: REQUEST_TIME_IN_MINUTES = 1_ip
    integer (ip), parameter :: REQUEST_TIME_IN_HOURS   = 2_ip
    !---------------------------------------------------------------------------------

    ! Declare derived data type
    type, public :: CpuTimer
        !---------------------------------------------------------------------------------
        ! Class variables
        !---------------------------------------------------------------------------------
        logical,      private :: initialized     = .false.
        logical,      private :: timer_started   = .false.
        logical,      private :: timer_stopped   = .false.
        real (wp),    private :: cpu_start_time  = 0.0_wp
        real (wp),    private :: cpu_finish_time = 0.0_wp
        integer (ip), private :: initial_ticks   = 0_ip
        integer (ip), private :: final_ticks     = 0_ip  ! final value of the clock tick counter
        integer (ip), private :: count_max       = 0_ip  ! maximum value of the clock counter
        integer (ip), private :: count_rate      = 0_ip  ! number of clock ticks per second
        integer (ip), private :: num_ticks       = 0_ip  ! number of clock ticks of the code
        !---------------------------------------------------------------------------------
    contains
        !---------------------------------------------------------------------------------
        ! Class methods
        !---------------------------------------------------------------------------------
        procedure, public         :: start => start_cpu_timer
        procedure, public         :: stop  => stop_cpu_timer
        procedure, public         :: get_total_cpu_time
        procedure, public         :: get_elapsed_time
        procedure, public, nopass :: print_time_stamp
        procedure, private        :: create  => initialize_cpu_timer
        procedure, private        :: destroy => destruct_cpu_timer
        final                     :: finalize_cpu_timer
        !---------------------------------------------------------------------------------
    end type CpuTimer

contains
    !
    !*****************************************************************************************
    !
    subroutine start_cpu_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out) :: this
        !--------------------------------------------------------------------------------

        ! Initialize timer
        call this%create()

        ! Set CPU start time
        call cpu_time( this%cpu_start_time )

        ! Set initial ticks
        call system_clock( count=this%initial_ticks )

        ! Set timer status
        this%timer_started = .true.

    end subroutine start_cpu_timer
    !
    !*****************************************************************************************
    !
    subroutine stop_cpu_timer( this )
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

    end subroutine stop_cpu_timer
    !
    !*****************************************************************************************
    !
    function get_total_cpu_time( this, units ) result( return_value )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out)       :: this
        integer (ip),     intent (in), optional :: units
        real (wp)                               :: return_value
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Return zero if the timer was never started
        !--------------------------------------------------------------------------------

        if ( .not.this%timer_started ) then
            return_value = 0.0_wp
            return
        end if

        !--------------------------------------------------------------------------------
        ! If the timer was not stopped, then return the current time elapsed
        !--------------------------------------------------------------------------------

        if ( .not.this%timer_stopped ) then

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

        if ( present( units ) ) then
            select case ( units )
                case( REQUEST_TIME_IN_MINUTES )
                    return_value = return_value/ 60
                case( REQUEST_TIME_IN_HOURS )
                    return_value = return_value/ 3600
                case default
                    write( stderr, '(A)') 'TYPE(CpuTimer)'
                    write( stderr, '(A)' ) 'Invalid calling argument for UNITS'
                    write( stderr, '(A)' ) 'must be either 0, 1, or 2'
            end select
        end if

    end function get_total_cpu_time
    !
    !*****************************************************************************************
    !
    function get_elapsed_time( this, units ) result ( return_value )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out)       :: this
        integer (ip),     intent (in), optional :: units
        real (wp)                               :: return_value
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Return zero if the timer was never started
        !--------------------------------------------------------------------------------

        if ( .not.this%timer_started ) then
            return_value = 0.0_wp
            return
        end if

        !--------------------------------------------------------------------------------
        ! If the timer was not stopped, then return the current time elapsed
        !--------------------------------------------------------------------------------

        if ( .not.this%timer_stopped ) then

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

            return_value = real( num, kind=wp) / count_rate

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
                    write( stderr, '(A)') 'TYPE(CpuTimer)'
                    write( stderr, '(A)' ) 'Invalid calling argument for UNITS'
                    write( stderr, '(A)' ) 'must be either 0, 1, or 2'
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
        integer (ip), intent (in), optional :: file_unit
        !--------------------------------------------------------------------------------
        ! Dictionary: local variables
        !--------------------------------------------------------------------------------
        integer (ip)                   :: values(8)
        integer (ip)                   :: unit
        character (len=10)             :: time
        character (len=5)              :: zone
        character (len=8)              :: am_or_pm
        character (len=8)              :: date
        character (len=:), allocatable :: list_of_months(:)
        character (len=:), allocatable :: time_format
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Set months
        !--------------------------------------------------------------------------------

        allocate( &
            list_of_months(12), &
            source = [ &
            'January  ', 'February ', 'March    ', 'April    ', &
            'May      ', 'June     ', 'July     ', 'August   ', &
            'September', 'October  ', 'November ', 'December ' ] &
            )

        !--------------------------------------------------------------------------------
        ! Set time format
        !--------------------------------------------------------------------------------

        allocate( &
            time_format, &
            source = '( A, 1X, I2, 1X, I4, 2X, I2, A1, I2.2, A1, I2.2, A1, I3.3, 1X, A)' &
            )

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
            ! Set write unit
            !--------------------------------------------------------------------------------

            if ( present( file_unit )) then
                unit = file_unit
            else
                unit = stdout
            end if

            !--------------------------------------------------------------------------------
            ! Return time stamp
            !--------------------------------------------------------------------------------

            write ( unit, fmt = time_format ) &
                trim( LIST_OF_MONTHS( month ) ), &
                day, year, hour, ':', &
                minute, ':', second, '.', &
                millisecond, trim( am_or_pm )

        end associate

        !--------------------------------------------------------------------------------
        ! Free memory
        !--------------------------------------------------------------------------------

        deallocate( list_of_months, time_format )

    end subroutine print_time_stamp
    !
    !*****************************************************************************************
    !
    subroutine initialize_cpu_timer( this )
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

            call this%destroy()

        end if

        !--------------------------------------------------------------------------------
        ! Initialize counters
        !--------------------------------------------------------------------------------

        call system_clock( count_rate=this%count_rate,  count_max=this%count_max )

    end subroutine initialize_cpu_timer
    !
    !*****************************************************************************************
    !
    subroutine destruct_cpu_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (CpuTimer), intent (in out) :: this
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Check status
        !--------------------------------------------------------------------------------

        if ( .not.this%initialized ) return

        !---------------------------------------------------------------------------------
        ! Reset booleans
        !---------------------------------------------------------------------------------

        this%initialized = .false.
        this%timer_started = .false.
        this%timer_stopped = .false.

        !---------------------------------------------------------------------------------
        ! Reset floats
        !---------------------------------------------------------------------------------

        this%cpu_start_time = 0.0_wp
        this%cpu_finish_time = 0.0_wp

        !---------------------------------------------------------------------------------
        ! Reset integers
        !---------------------------------------------------------------------------------

        this%initial_ticks = 0_ip
        this%final_ticks = 0_ip
        this%count_max = 0_ip
        this%count_rate = 0_ip
        this%num_ticks = 0_ip

    end subroutine destruct_cpu_timer
    !
    !*****************************************************************************************
    !
    subroutine finalize_cpu_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        type (CpuTimer), intent (in out) :: this
        !--------------------------------------------------------------------------------

        call this%destroy()

    end subroutine finalize_cpu_timer
    !
    !*****************************************************************************************
    !
end module type_CpuTimer
