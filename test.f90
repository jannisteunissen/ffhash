
module m_khash
  use iso_fortran_env, dp => real64
  implicit none
#define KEY_TYPE integer(int32)
#define VAL_TYPE integer(int32)
#include "m_khash.f90"
end module m_khash

program test
  use m_khash

  implicit none

  type(khash_t) :: h
  integer :: i

  i = khash_put(h, 42)
  i = khash_put(h, 41)
  i = khash_put(h, 40)
  i = khash_put(h, 39)
  i = khash_put(h, 38)
  print *, i
  print *, "n_buckets", h%n_buckets
  print *, i
  print *, h%keys(i)
  i = khash_get(h, 40)
  print *, i, h%keys(i)

end program test
