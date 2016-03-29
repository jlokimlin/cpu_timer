module cpu_timer_library

    use type_CpuTimer, only: &
        CpuTimer

    ! Explicit typing only
    implicit none

    ! Everything is private unless stated otherwise
    private
    public :: CpuTimer

end module cpu_timer_library
