module m_ffhash
#define FFH_KEY_TYPE character(len=20)
#define FFH_KEY_IS_STRING
#define FFH_VAL_TYPE integer
#define FFH_CUSTOM_HASH_FUNCTION
#include "ffhash_inc.f90"

  pure integer function hash_function(key) result(hash)
    character(len=*), intent(in) :: key
    integer                      :: n

    hash = 5381
    do n = 1, len(key)
       hash = (shiftl(hash, 5) + hash) + iachar(key(n:n)) ! hash * 33 + c
    end do
  end function hash_function
end module m_ffhash

program example
  use m_ffhash

  implicit none

  type(ffh_t) :: h
  integer     :: day, month, year, status

  call h%store_value("day", 20, status)
  call h%store_value("month", 2, status)
  call h%store_value("year", 2020, status)

  call h%get_value("day", day, status)
  call h%get_value("month", month, status)
  call h%get_value("year", year, status)

  print *, day, month, year
  print *, h%get_index("day"), h%get_index("month"), h%get_index("year")

end program example
