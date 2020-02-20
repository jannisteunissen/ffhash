module m_ffhash
#define FFH_KEY_TYPE integer
#include "ffhash_inc.f90"
end module m_ffhash

program benchmark_integer
  use iso_fortran_env, dp => real64
  use m_ffhash

  implicit none

  type(ffh_t)        :: h
  integer, parameter   :: n_max = 5*1000*1000
  integer              :: n, i, status
  integer              :: keys(n_max)
  integer, allocatable :: key_counts(:)
  real(dp)             :: r_uniform(n_max), t_start, t_end

  call random_number(r_uniform)
  keys = nint(r_uniform * n_max * 0.25_dp)

  call cpu_time(t_start)
  do n = 1, n_max
     i = ffh_get_index(h, keys(n))

     if (i /= -1) then
        call ffh_delete_index(h, i, status)
     else
        i = ffh_store_key(h, keys(n))
     end if
  end do
  call cpu_time(t_end)

  print *, "Elapsed time", t_end - t_start
  print *, "Size/n_occupied/n_buckets", h%n_keys_stored, h%n_occupied, h%n_buckets

  ! Count number of keys that occur an odd number of times
  allocate(key_counts(minval(keys):maxval(keys)))
  key_counts = 0
  do n = 1, n_max
     key_counts(keys(n)) = key_counts(keys(n)) + 1
  end do
  n = sum(iand(key_counts, 1))

  if (n /= h%n_keys_stored) then
     error stop "Incorrect h%size"
  else
     print *, "Test passed (h%size is correct)"
  end if

end program benchmark_integer
