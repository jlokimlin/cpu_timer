## CPU_timer

An object-oriented approach for timing program execution in modern Fortran.

## Usage:

```fortran

use, intrinsic :: iso_fortran_env, only: &
    wp => REAL64, &
    ip => INT32

use type_CpuTimer, only: &
    CpuTimer

! Explicit typing only
implicit none

type (CpuTimer)            :: timer
real (wp)                  :: wall_clock_time
real (wp)	               :: total_processor_time
integer (ip), parameter    :: UNITS = 0 ! (optional argument) = 0 for seconds, or 1 for minutes, or 2 for hours

!*starting the timer

call timer%start()

!* stopping the timer

call timer%stop()

!* Reading the time

wall_clock_time = timer%get_elapsed_time( UNITS )

total_processor_time = timer%get_total_cpu_time( UNITS )

!* Put a time stamp

call timer%print_time_stamp()

```