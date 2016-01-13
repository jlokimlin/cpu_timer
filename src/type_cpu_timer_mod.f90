!*****************************************************************************************
!
!< Purpose:
!< Defines an object for timing program execution
!
!  Usage:
!
!         type (cpu_timer_t) :: timer
!         real (WP)          :: t
!         integer (IP)       :: units
!
!         !*Starting the timer
!
!               call timer%Start()
!
!         !* Stopping the timer
!
!               call timer%Stop()
!
!         !* Reading the time
!
!               t = timer%Get_elapsed_time( units ) ! Wall clock time
!               t = timer%Get_total_time( units )   ! Total processor time
!
!         !* units (optional) = 0 for seconds, or 1 for minutes, or 2 for hours
!
!         !* Get time stamp
!
!               call timer%Get_time_stamp()
!
!*****************************************************************************************
!
module type_cpu_timer_mod

    use, intrinsic :: iso_fortran_env, only: &
        WP     => REAL64, &
        IP     => INT32, &
        stdout => OUTPUT_UNIT

    ! Explicit typing only
    implicit none

    ! Everything is private unless stated otherwise
    private
    public :: cpu_timer_t

    !---------------------------------------------------------------------------------
    ! Dictionary: global variables confined to the module
    !---------------------------------------------------------------------------------
    integer (IP), parameter  :: REQUEST_TIME_IN_SECONDS = 0_IP
    integer (IP), parameter  :: REQUEST_TIME_IN_MINUTES = 1_IP
    integer (IP), parameter  :: REQUEST_TIME_IN_HOURS   = 2_IP
    !---------------------------------------------------------------------------------

    ! Declare derived data type
    type, public :: cpu_timer_t

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
        procedure, public         :: Start => Start_timer
        procedure, public         :: Stop  => Stop_timer
        procedure, public         :: Get_total_time
        procedure, public         :: Get_elapsed_time
        procedure, public, nopass :: Print_time_stamp
        !---------------------------------------------------------------------------------
        ! Private methods
        !---------------------------------------------------------------------------------
        procedure                 :: Create  => Initialize_timer
        procedure                 :: Destroy => Destruct_timer
        !---------------------------------------------------------------------------------
        ! Finalizer
        !---------------------------------------------------------------------------------
        !final                     :: Finalize_timer
        !---------------------------------------------------------------------------------

    end type cpu_timer_t

contains
    !
    !*****************************************************************************************
    !
    ! Public methods
    !
    !*****************************************************************************************
    !
    subroutine Start_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (cpu_timer_t), intent (in out) :: this
        !--------------------------------------------------------------------------------

        ! Initialize timer
        call this%Create()

        ! Set CPU start time
        call cpu_time( this%cpu_start_time )

        ! Set initial ticks
        call system_clock( count = this%initial_ticks )

        ! Set timer status
        this%timer_started = .true.

    end subroutine Start_timer
    !
    !*****************************************************************************************
    !
    subroutine Stop_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (cpu_timer_t), intent (in out) :: this
        !--------------------------------------------------------------------------------

        ! Check timer flag
        if ( .not. this%timer_started ) return

        ! Set cpu finihs time
        call cpu_time( this%cpu_finish_time )

        ! Set final ticks
        call system_clock( count = this%final_ticks )

        ! Set timer status
        this%timer_stopped = .true.

    end subroutine Stop_timer
    !
    !*****************************************************************************************
    !
    function Get_total_time( this, units ) result( return_value )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (cpu_timer_t), intent (in out)        :: this
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

            call this%Stop()

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

    end function Get_total_time
    !
    !*****************************************************************************************
    !
    function Get_elapsed_time( this, units ) result ( return_value )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (cpu_timer_t), intent (in out)        :: this
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

            call this%Stop()

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

                    return_value = return_value/60

                case( REQUEST_TIME_IN_HOURS )

                    return_value = return_value/3600

                case default

            end select

        end if

    end function Get_elapsed_time
    !
    !*****************************************************************************************
    !
    subroutine Print_time_stamp( )
        !
        !< Purpose:
        !
        !  Prints the current YMDHMS date as a time stamp.
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
        character (*), parameter  :: TIME_FORMAT = &
            '( A, 1x, i2, 1x, i4, 2x, i2, a1, i2.2, a1, i2.2, a1, i3.3, 1x, a)'
        !--------------------------------------------------------------------------------

        ! Get the corresponding date and time information from the real-time system clock.
        call date_and_time( date, time, zone, values )

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

            write ( stdout, TIME_FORMAT ) &
                trim( LIST_OF_MONTHS(month) ), &
                day, year, hour, ':', &
                minute, ':', second, '.', millisecond, trim( am_or_pm )

        end associate

    end subroutine Print_time_stamp
    !
    !*****************************************************************************************
    !
    ! Private methods
    !
    !*****************************************************************************************
    !
    subroutine Initialize_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (cpu_timer_t), intent (in out) :: this
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

    end subroutine Initialize_timer
    !
    !*****************************************************************************************
    !
    subroutine Destruct_timer( this )
        !
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        class (cpu_timer_t), intent (in out) :: this
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

    end subroutine Destruct_timer
    !
    !*****************************************************************************************
    !
    ! Finalizer !! GCC 4.8.2 doesn't support finalization
    !
    !*****************************************************************************************
    !
!    subroutine Finalize_timer( this )
!        !
!        !--------------------------------------------------------------------------------
!        ! Dictionary: calling arguments
!        !--------------------------------------------------------------------------------
!        type (cpu_timer_t), intent (in out) :: this
!        !--------------------------------------------------------------------------------
!
!        call this%Destroy()
!
!    end subroutine Finalize_timer
    !
    !*****************************************************************************************
    !
end module type_cpu_timer_mod
