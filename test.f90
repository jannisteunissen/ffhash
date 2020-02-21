module m_ffhash
#define FFH_KEY_TYPE integer
#define FFH_VAL_TYPE integer
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
     call h%store_value(keys(n), values(n), i)
  end do

  do n = 1, n_max/2
     call h%delete_key(keys(n), status)
  end do

  do n = 1, n_max
     call h%store_value(keys(n), values(n), i)
  end do

  do n = 1, n_max
     i = h%get_index(keys(n))
     if (i /= -1) then
        print *, "RESULT", n, i, keys(n), h%vals(i), values(n)
     end if
  end do

end program test
