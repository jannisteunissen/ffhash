#define KEY_TYPE integer(int32)
#define VAL_TYPE integer(int32)
#include "m_khash.f90"

program test
  use m_khash

  implicit none

  type(khash_t)      :: h
  integer, parameter :: n_max = 10
  integer            :: n, i, status
  integer            :: keys(n_max), values(n_max)

  do n = 1, n_max
     keys(n) = n**2
     values(n) = -n
  end do

  do n = 1, n_max
     i = khash_put(h, keys(n))
     h%vals(i) = values(n)
  end do

  do n = 1, n_max/2
     i = khash_get(h, keys(n))
     call khash_del(h, i, status)
  end do

  do n = 1, n_max
     i = khash_put(h, keys(n))
     h%vals(i) = values(n)
  end do

  do n = 1, n_max
     i = khash_get(h, keys(n))
     if (i /= -1) then
        print *, "RESULT", n, i, keys(n), h%vals(i), values(n)
     end if
  end do

end program test
