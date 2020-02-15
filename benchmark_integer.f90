#define KEY_TYPE integer(int32)
#define VAL_TYPE integer(int32)
#include "m_khash.f90"

program benchmark_integer
  use iso_fortran_env, dp => real64
  use m_khash

  implicit none

  type(khash_t)      :: h
  integer, parameter :: n_max = 5*1000*1000
  integer            :: n, i, status, my_unit
  integer            :: keys(n_max)
  real(dp)           :: r_uniform(n_max), t_start, t_end

  call random_number(r_uniform)
  keys = nint(r_uniform * n_max * 0.25_dp)

  call cpu_time(t_start)
  do n = 1, n_max
     i = khash_get(h, keys(n))

     if (i /= -1) then
        call khash_del(h, i, status)
     else
        i = khash_put(h, keys(n))
     end if
  end do
  call cpu_time(t_end)

  print *, t_end - t_start

  print *, "size/n_occupied/n_buckets", h%size, h%n_occupied, h%n_buckets

  open(newunit=my_unit, file="integers.txt", action='write')
  do n = 1, n_max
     write(my_unit, "(I0)") keys(n)
  end do
  close(my_unit)

end program benchmark_integer
