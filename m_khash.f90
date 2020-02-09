!    Fortran implementation of khash by Jannis Teunissen
!
!    Original khash license copied below:
!
!    The MIT License
!
!    Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>

!    Permission is hereby granted, free of charge, to any person obtaining
!    a copy of this software and associated documentation files (the
!    "Software"), to deal in the Software without restriction, including
!    without limitation the rights to use, copy, modify, merge, publish,
!    distribute, sublicense, and/or sell copies of the Software, and to
!    permit persons to whom the Software is furnished to do so, subject to
!    the following conditions:

!    The above copyright notice and this permission notice shall be
!    included in all copies or substantial portions of the Software.

!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
!    BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
!    ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!    SOFTWARE.
  implicit none

  double precision, parameter :: HASH_UPPER = 0.77

  type khash_t
     integer                     :: n_buckets   = 0
     integer                     :: size        = 0
     integer                     :: n_occupied  = 0
     integer                     :: upper_bound = 0
     integer(int32), allocatable :: flags(:)
     KEY_TYPE, allocatable       :: keys(:)
     VAL_TYPE, allocatable       :: vals(:)
  end type khash_t

contains

  function khash_get(h, key) result(ix)
    type(khash_t), intent(in) :: h
    KEY_TYPE, intent(in)      :: key
    integer                   :: ix, i, k, last, mask

    ix = -1

    if (h%n_buckets < 1) return

    mask = h%n_buckets - 1
    k = hash_function(key)
    i = iand(k, mask)
    last = i

    do while (.not. isempty(h, i) .and. (isdel(h, i) .or. &
         h%keys(i) == key))
       i = iand(i + 1, mask)
       if (i == last) return
    end do

    if (iseither(h, i)) return
    ix = i
  end function khash_get

  function khash_put(h, key) result(ix)
    type(khash_t), intent(in) :: h
    KEY_TYPE, intent(in)      :: key
    integer                   :: ix, i, k, last, mask

    ix = -1

    if (h%n_occupied >= h%upper_bound) then
       if (h%n_buckets > shiftr(h%size, 1)) then
          ! Enough free space, but need to clean up the table
          call khash_resize(h, h%n_buckets, status)
          if (status /= 0) return
       else
          ! Increase table size
          call khash_resize(h, 2*h%n_buckets, status)
          if (status /= 0) return
       end if
    end if

    i = hash_index(h, key)

    ! Skip over filled slots if they are deleted or have the wrong key. Skipping
    ! over deleted slots ensures that a key is not added twice, in case it is
    ! not at its 'first' hash index, and some keys in between have been deleted.
    do while (.not. isempty(h, i) .and. &
         (isdel(h, i) .or. .not. h%keys(i) == key))
       if (isdel(h, i)) site = i
       step = step + 1
       i = iand(i + step, mask) + 1
    end do

    if (isempty(h, i)) then
       h%keys(i) = key
       call set_isboth_false(h, i)
       h%size = h%size + 1
       h%n_occupied = h%n_occupied + 1
    else if (isdel(h, i)) then
       
    end if
  end function khash_put

  subroutine khash_resize(h, new_n_buckets, status)
    type(khash_t), intent(inout) :: h
    integer, intent(in)          :: new_n_buckets
    integer, intent(out)         :: status
    integer                      :: n_new
    logical                      :: rehashing
    type(khash_t)                :: hcopy

    ! Make sure n_new is a power of two, and at least 4
    n_new = 4
    do while (n_new < new_n_buckets)
       n_new = 2 * n_new
    end do

    if (h%size >= ceiling(n_new * HASH_UPPER)) then
       ! Requested size is too small
       status = 1
       return
    end if

    ! Expand or shrink table
    call move_alloc(h%flags, hcopy%flags)
    call move_alloc(h%keys, hcopy%keys)
    call move_alloc(h%vals, hcopy%vals)
    allocate(h%flags(n_new), h%keys(n_new), h%values(n_new), stat=status)
    if (status /= 0) return
    h%flags(:) = 0

    new_mask = n_new - 1

    do j = 1, h%n_buckets
       if (.not. iseither(hcopy, j)) then
          ! Find a new index
          k = hash_function(hcopy%keys(j))
          i = iand(k, new_mask) + 1

          do while (.not. isempty(h, i))
             step = step + 1
             i = iand(i+step, new_mask) + 1
          end do

          h%vals(i) = hcopy%vals(j)
          h%keys(i) = hcopy%keys(j)
          call set_isempty_false(h, i)
       end if
    end do

    h%n_buckets = n_new
    h%n_occupied = h%size
    h%upper_bound = ceiling(h%n_buckets * HASH_UPPER)

    status = 0
  end subroutine khash_resize

  pure logical function isempty(h, i)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i
    isempty = (iand(h%flags(i), 1) /= 0)
  end function isempty

  pure logical function isdel(h, i)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i
    isdel = (iand(h%flags(i), 2) /= 0)
  end function isdel

  pure logical function iseither(h, i)
    type(khash_t), intent(in) :: h
    integer, intent(in)       :: i
    iseither = (iand(h%flags(i), 3) /= 0)
  end function iseither

#ifndef hash_function
  pure function hash_function(key) result(hash)
    KEY_TYPE, intent(in)   :: key
    integer(int32)         :: hash, n
    integer, parameter     :: n_bytes = ceiling(bit_size(key)*0.125d0)
    character(len=n_bytes) :: buf

    buf = transfer(key, buf)
    hash = 5381

    do n = 1, n_bytes
       hash = (shiftl(hash, 5) + hash) + iachar(buf(n:n)) ! hash * 33 + char
    end do
  end function hash_function
#endif

