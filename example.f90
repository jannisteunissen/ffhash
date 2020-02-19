module m_ffhash
#define KEY_TYPE character(len=20)
#define VAL_TYPE integer
#include "ffhash_inc.f90"
end module m_ffhash

program example
  use m_ffhash

  implicit none

  type(ffh_t) :: h
  character(len=20) :: string
  integer :: day, month, year, status

  string = "day"
  call ffh_store_value(h, string, 20, status)
  string = "month"
  call ffh_store_value(h, string, 2, status)
  string = "year"
  call ffh_store_value(h, string, 2020, status)

  string = "day"
  call ffh_get_value(h, string, day, status)
  string = "month"
  call ffh_get_value(h, string, month, status)
  string = "year"
  call ffh_get_value(h, string, year, status)

  print *, day, month, year
end program example
