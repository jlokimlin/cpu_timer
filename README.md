## CPU_timer

An object-oriented approach for timing program execution in modern Fortran (2008+).

## Usage:

```fortran

use, intrinsic :: iso_fortran_env, only: &
    WP     => REAL64, &
    IP     => INT32

use type_CpuTimer, only: &
    CpuTimer

! Explicit typing only
implicit none

type (CpuTimer)            :: timer
real (WP)                  :: wall_clock_time
real (WP)	               :: total_processor_time
integer (IP), parameter    :: UNITS = 0 ! (optional argument) = 0 for seconds, or 1 for minutes, or 2 for hours

!*starting the timer

call timer%start()

!* stopping the timer

call timer%stop()

!* Reading the time

wall_clock_time = timer%get_elapsed_time( UNITS )

total_processor_time = timer%get_total_time( UNITS )

!* Get time stamp

call timer%print_time_stamp()

```