module m_ffhash
  implicit none
#define FFH_KEY_TYPE character(len=20)
#define FFH_KEY_IS_STRING
#define FFH_VAL_TYPE integer
#define FFH_CUSTOM_HASH_FUNCTION
#include "ffhash_inc.f90"

  ! djb2 hash function
  pure integer function hash_function(key) result(hash)
    character(len=*), intent(in) :: key
    integer                      :: n

    hash = 5381
    do n = 1, len(key)
       ! hash * 33 + c
       hash = (shiftl(hash, 5) + hash) + iachar(key(n:n))
    end do
  end function hash_function
end module m_ffhash

program test
  use m_ffhash

  implicit none

  type(ffh_t) :: h
  integer     :: day, month, year

  call h%ustore_value("day", 20)
  call h%ustore_value("month", 2)
  call h%ustore_value("year", 2020)

  call h%uget_value("day", day)
  call h%uget_value("month", month)
  call h%uget_value("year", year)

  call h%uget_value("yeararst", year)

  if (all([day, month, year] == [20, 2, 2020])) then
     print *, [day, month, year]
     print *, h%get_index(["day  ", "month", "year "])
     print *, "PASSED"
  else
     error stop "FAILED"
  end if

  call h%reset()
end program test
