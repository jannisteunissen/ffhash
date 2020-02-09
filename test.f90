
module m_khash
  use iso_fortran_env, dp => real64
#define KEY_TYPE integer(int32)
#define VAL_TYPE integer(int64)
#include "m_khash.f90"
end module m_khash

program test
  use m_khash

  implicit none
  ! include 'test.inc'
! #include "test.inc"

end program test
