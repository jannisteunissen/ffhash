module m_ffhash
#define KEY_TYPE integer
#define VAL_TYPE integer
#include "ffhash_inc.f90"
end module m_ffhash

program test
  use m_ffhash

  implicit none

  type(ffh_t)      :: h
  integer, parameter :: n_max = 10
  integer            :: n, i, status
  integer            :: keys(n_max), values(n_max)

  do n = 1, n_max
     keys(n) = n**2
     values(n) = -n
  end do

  do n = 1, n_max
     call ffh_store_value(h, keys(n), values(n), i)
  end do

  do n = 1, n_max/2
     call ffh_delete_key(h, keys(n), status)
  end do

  do n = 1, n_max
     call ffh_store_value(h, keys(n), values(n), i)
  end do

  do n = 1, n_max
     i = ffh_get_index(h, keys(n))
     if (i /= ffh_fail) then
        print *, "RESULT", n, i, keys(n), h%vals(i), values(n)
     end if
  end do

end program test
