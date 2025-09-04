module m_ffhash
  use iso_fortran_env
  implicit none
#define FFH_KEY_TYPE integer
#include "ffhash_inc.f90"
end module m_ffhash

module m_ffhash_64
  use iso_fortran_env
  implicit none
#define FFH_KEY_TYPE integer
#define FFH_ENABLE_INT64
#include "ffhash_inc.f90"
end module m_ffhash_64

program test
  use iso_fortran_env
  implicit none

  integer, parameter    :: n_max = 5*1000*1000

  call test_32(n_max)
  call test_64(n_max)

contains

  subroutine test_32(n_max)
    use iso_fortran_env, dp => real64
    use m_ffhash
    integer, intent(in) :: n_max

    type(ffh_t)           :: h
    integer               :: n, i, status
    integer, allocatable  :: keys(:)
    integer, allocatable  :: key_counts(:)
    real(dp)              :: t_start, t_end
    real(dp), allocatable :: r_uniform(:)

    allocate(keys(n_max), r_uniform(n_max))

    call random_number(r_uniform)
    keys = nint(r_uniform * n_max * 0.25_dp)

    call cpu_time(t_start)
    do n = 1, n_max
       i = h%get_index(keys(n))

       if (i /= -1) then
          call h%delete_index(i, status)
       else
          call h%store_key(keys(n), i)
       end if
    end do
    call cpu_time(t_end)

    write(*, "(A)") "Results for 32-bit hashing and indexing"
    write(*, "(A,E12.4)") "Elapsed time (s) ", t_end - t_start
    write(*, "(A,E12.4)") "Entries/s        ", n_max/(t_end - t_start)
    write(*, "(A,I12)")   "n_keys_stored    ", h%n_keys_stored
    write(*, "(A,I12)")   "n_occupied       ", h%n_occupied
    write(*, "(A,I12)")   "n_buckets        ", h%n_buckets

    ! Count number of keys that occur an odd number of times
    allocate(key_counts(minval(keys):maxval(keys)))
    key_counts = 0
    do n = 1, n_max
       key_counts(keys(n)) = key_counts(keys(n)) + 1
    end do
    n = sum(iand(key_counts, 1))

    if (n /= h%n_keys_stored) then
       error stop "FAILED"
    else
       print *, "PASSED"
    end if

    ! Clean up allocated storage
    call h%reset()
    deallocate(key_counts)
  end subroutine test_32

  subroutine test_64(n_max)
    use iso_fortran_env, dp => real64
    use m_ffhash_64
    integer, intent(in) :: n_max

    type(ffh_t)           :: h
    integer(int64)        :: n, i, status
    integer, allocatable  :: keys(:)
    integer, allocatable  :: key_counts(:)
    real(dp)              :: t_start, t_end
    real(dp), allocatable :: r_uniform(:)

    allocate(keys(n_max), r_uniform(n_max))

    call random_number(r_uniform)
    keys = nint(r_uniform * n_max * 0.25_dp)

    call cpu_time(t_start)
    do n = 1, n_max
       i = h%get_index(keys(n))

       if (i /= -1) then
          call h%delete_index(i, status)
       else
          call h%store_key(keys(n), i)
       end if
    end do
    call cpu_time(t_end)

    write(*, "(A)") "Results for 64-bit hashing and indexing"
    write(*, "(A,E12.4)") "Elapsed time (s) ", t_end - t_start
    write(*, "(A,E12.4)") "Entries/s        ", n_max/(t_end - t_start)
    write(*, "(A,I12)")   "n_keys_stored    ", h%n_keys_stored
    write(*, "(A,I12)")   "n_occupied       ", h%n_occupied
    write(*, "(A,I12)")   "n_buckets        ", h%n_buckets

    ! Count number of keys that occur an odd number of times
    allocate(key_counts(minval(keys):maxval(keys)))
    key_counts = 0
    do n = 1, n_max
       key_counts(keys(n)) = key_counts(keys(n)) + 1
    end do
    n = sum(iand(key_counts, 1))

    if (n /= h%n_keys_stored) then
       error stop "FAILED"
    else
       print *, "PASSED"
    end if

    ! Clean up allocated storage
    call h%reset()
    deallocate(key_counts)
  end subroutine test_64

end program
