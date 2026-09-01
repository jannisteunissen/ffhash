module m_ffhash
  use iso_fortran_env
  implicit none
#define FFH_KEY_TYPE integer
#define FFH_VAL_TYPE integer
#include "ffhash_inc.f90"
end module m_ffhash

module m_ffhash_str
  use iso_fortran_env
  implicit none
#define FFH_STRING_KEY_TYPE character(len=20)
#define FFH_VAL_TYPE integer
#include "ffhash_inc.f90"
end module m_ffhash_str

program test_ffhash
  use iso_fortran_env, only: int32
  use m_ffhash, ffh_t => ffh_t
  use m_ffhash_str, only: ffh_str_t => ffh_t
  implicit none

  integer :: n_failed = 0

  call test_basic_store_get()
  call test_overwrite()
  call test_missing_key()
  call test_delete()
  call test_delete_index()
  call test_many_keys()
  call test_reset()
  call test_get_value_or()
  call test_existing_key_error()
  call test_string_keys()

  if (n_failed == 0) then
     print *, "ALL TESTS PASSED"
  else
     print *, "FAILED TESTS:", n_failed
     error stop 1
  end if

contains

  subroutine check(cond, name)
    logical, intent(in)          :: cond
    character(len=*), intent(in) :: name
    if (cond) then
       print '(A,A)', "  PASS: ", name
    else
       print '(A,A)', "  FAIL: ", name
       n_failed = n_failed + 1
    end if
  end subroutine check

  subroutine test_basic_store_get()
    type(ffh_t) :: h
    integer     :: val, status
    print *, "== test_basic_store_get =="
    call h%ustore_value(10, 100)
    call h%ustore_value(20, 200)
    call h%ustore_value(30, 300)
    call check(h%n_keys_stored == 3, "n_keys_stored == 3")
    call h%get_value(10, val, status)
    call check(status >= 0 .and. val == 100, "get key 10 -> 100")
    call check(h%fget_value(20) == 200, "fget key 20 -> 200")
    call check(h%fget_value(30) == 300, "fget key 30 -> 300")
  end subroutine test_basic_store_get

  subroutine test_overwrite()
    type(ffh_t) :: h
    print *, "== test_overwrite =="
    call h%ustore_value(5, 50)
    call h%ustore_value(5, 55)
    call check(h%n_keys_stored == 1, "overwrite keeps 1 key")
    call check(h%fget_value(5) == 55, "value updated to 55")
  end subroutine test_overwrite

  subroutine test_missing_key()
    type(ffh_t) :: h
    integer     :: val, status
    print *, "== test_missing_key =="
    call h%ustore_value(1, 11)
    call check(h%get_index(999) == -1, "missing key index == -1")
    call h%get_value(999, val, status)
    call check(status < 0, "get missing -> status < 0")
    call check(h%fget_value_or(999, -1) == -1, "fget_value_or returns default")
    call check(h%fget_value_or(1, -1) == 11, "fget_value_or returns found")
  end subroutine test_missing_key

  subroutine test_delete()
    type(ffh_t) :: h
    integer     :: status
    print *, "== test_delete =="
    call h%ustore_value(7, 70)
    call h%ustore_value(8, 80)
    call h%delete_key(7, status)
    call check(status == 0, "delete existing -> status 0")
    call check(h%n_keys_stored == 1, "n_keys_stored == 1 after delete")
    call check(h%get_index(7) == -1, "deleted key not found")
    call check(h%fget_value(8) == 80, "other key still present")
    call h%delete_key(7, status)
    call check(status < 0, "delete already deleted -> status < 0")
  end subroutine test_delete

  subroutine test_delete_index()
    type(ffh_t) :: h
    integer     :: ix, status
    print *, "== test_delete_index =="
    call h%ustore_value(42, 420)
    ix = h%get_index(42)
    call check(h%valid_index(ix), "valid_index true for stored key")
    call h%delete_index(ix, status)
    call check(status == 0, "delete_index -> status 0")
    call check(.not. h%valid_index(ix), "valid_index false after delete")
    call h%delete_index(-5, status)
    call check(status < 0, "delete out-of-bounds index -> status < 0")
  end subroutine test_delete_index

  subroutine test_many_keys()
    type(ffh_t) :: h
    integer     :: i
    logical     :: ok
    print *, "== test_many_keys =="
    do i = 1, 1000
       call h%ustore_value(i, i*3)
    end do
    call check(h%n_keys_stored == 1000, "1000 keys stored")
    ok = .true.
    do i = 1, 1000
       if (h%fget_value(i) /= i*3) ok = .false.
    end do
    call check(ok, "all 1000 values correct (triggers resizes)")
  end subroutine test_many_keys

  subroutine test_reset()
    type(ffh_t) :: h
    print *, "== test_reset =="
    call h%ustore_value(1, 1)
    call h%ustore_value(2, 2)
    call h%reset()
    call check(h%n_keys_stored == 0, "reset -> 0 keys")
    call check(h%n_buckets == 0, "reset -> 0 buckets")
    call h%ustore_value(3, 30)
    call check(h%fget_value(3) == 30, "usable after reset")
  end subroutine test_reset

  subroutine test_get_value_or()
    type(ffh_t) :: h
    print *, "== test_get_value_or =="
    call h%ustore_value(100, 999)
    call check(h%fget_value_or(100, 0) == 999, "found value")
    call check(h%fget_value_or(101, 0) == 0, "default value")
  end subroutine test_get_value_or

  subroutine test_existing_key_error()
    type(ffh_t) :: h
    integer     :: ix
    print *, "== test_existing_key_error =="
    call h%store_value(1, 10, ix, existing_key_is_error=.true.)
    call check(ix >= 0, "first store ok")
    call h%store_value(1, 20, ix, existing_key_is_error=.true.)
    call check(ix == -2, "duplicate store -> ix == -2")
    call check(h%fget_value(1) == 10, "value unchanged after failed store")
  end subroutine test_existing_key_error

  subroutine test_string_keys()
    type(ffh_str_t) :: h
    integer         :: status
    print *, "== test_string_keys =="
    call h%ustore_value("alpha", 1)
    call h%ustore_value("beta",  2)
    call h%ustore_value("gamma", 3)
    call check(h%fget_value("alpha") == 1, "string key alpha -> 1")
    call check(h%fget_value("beta")  == 2, "string key beta -> 2")
    call check(h%get_index("delta") == -1, "missing string key")
    call h%delete_key("beta", status)
    call check(status == 0 .and. h%get_index("beta") == -1, "delete string key")
  end subroutine test_string_keys

end program test_ffhash
