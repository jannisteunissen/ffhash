module m_ffhash
#define FFH_KEY_TYPE character(len=20)
#define FFH_KEY_IS_STRING
#define FFH_VAL_TYPE integer
#include "ffhash_inc.f90"
end module m_ffhash

program example
  use m_ffhash

  implicit none

  type(ffh_t) :: h
  integer     :: day, month, year, status

  call ffh_store_value(h, "day", 20, status)
  call ffh_store_value(h, "month", 2, status)
  call ffh_store_value(h, "year", 2020, status)

  call ffh_get_value(h, "day", day, status)
  call ffh_get_value(h, "month", month, status)
  call ffh_get_value(h, "year", year, status)

  print *, day, month, year
end program example
