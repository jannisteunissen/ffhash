  ! This file should be included in a module ... end module block
  ! For example:
  ! module m_ffhash
  ! #define KEY_TYPE integer
  ! #define VAL_TYPE integer (optional)
  ! #include "ffhash_inc.f90"
  ! end module m_ffhash

  !> Type storing the hash table
  type ffh_t
     !> Number of buckets in hash table
     integer                :: n_buckets       = 0
     !> Number of keys stored in hash table
     integer                :: n_keys_stored   = 0
     !> Number of keys stored plus deleted keys
     integer                :: n_occupied      = 0
     !> Maximum number of occupied buckets
     integer                :: n_occupied_max  = 0
     !> Mask to convert hash to index
     integer                :: hash_mask       = 0
     !> Maximum load factor for the hash table
     double precision       :: max_load_factor = 0.7d0
     !> Flags indicating whether buckets are empty or deleted
     character, allocatable :: flags(:)
     !> Keys of the hash table
     KEY_TYPE, allocatable  :: keys(:)
#ifdef VAL_TYPE
     !> Values stored for the keys
     VAL_TYPE, allocatable  :: vals(:)
#endif
  end type ffh_t

  !> Value to indicate a successful operation
  integer, parameter :: ffh_success = 0
  !> Value to indicate a failed operation
  integer, parameter :: ffh_fail = -1

  public :: ffh_success
  public :: ffh_fail
  public :: ffh_t

  public :: ffh_get_index
  public :: ffh_store_key
  public :: ffh_delete
  public :: ffh_valid_index
#ifdef VAL_TYPE
  public :: ffh_get_value
  public :: ffh_store_value
#endif

contains

  !> Get index corresponding to a key
  function ffh_get_index(h, key) result(ix)
    type(ffh_t), intent(in) :: h
    KEY_TYPE, intent(in)    :: key
    integer                 :: ix, i, step

    i = hash_index(h, key)

    do step = 1, h%n_buckets
       ! Exit when an empty bucket or the key is found
       if (bucket_empty(h, i) .or. &
            (.not. bucket_deleted(h, i) .and. h%keys(i) == key)) exit
       i = next_index(h, i, step)
    end do

    ix = ffh_fail
    if (step == h%n_buckets + 1) return ! Not found in loop
    if (.not. ffh_valid_index(h, i)) return  ! Exited, but key not found
    ix = i
  end function ffh_get_index

#ifdef VAL_TYPE
  !> Get the value corresponding to a key
  subroutine ffh_get_value(h, key, val, status)
    type(ffh_t), intent(in) :: h
    KEY_TYPE, intent(in)    :: key
    VAL_TYPE, intent(inout) :: val
    integer, intent(out)    :: status

    status = ffh_get_index(h, key)
    if (status /= ffh_fail) val = h%vals(status)
  end subroutine ffh_get_value

  !> Store the value corresponding to a key
  subroutine ffh_store_value(h, key, val, ix)
    type(ffh_t), intent(inout) :: h
    KEY_TYPE, intent(in)       :: key
    VAL_TYPE, intent(in)       :: val
    integer, intent(out)       :: ix !< Index (or ffh_fail)

    ix = ffh_store_key(h, key)
    if (ix /= ffh_fail) h%vals(ix) = val
  end subroutine ffh_store_value
#endif

  !> Store key in the table, and return index
  function ffh_store_key(h, key) result(i)
    type(ffh_t), intent(inout) :: h
    KEY_TYPE, intent(in)       :: key
    integer                    :: i, i_deleted, step, status

    i = ffh_fail

    if (h%n_occupied >= h%n_occupied_max) then
       if (h%n_keys_stored <= h%n_occupied_max/2) then
          ! Enough free space, but need to clean up the table
          call ffh_resize(h, h%n_buckets, status)
          if (status /= 0) return
       else
          ! Increase table size
          call ffh_resize(h, 2*h%n_buckets, status)
          if (status /= 0) return
       end if
    end if

    i = hash_index(h, key)

    if (.not. bucket_empty(h, i)) then
       i_deleted = -1
       ! Skip over filled slots if they are deleted or have the wrong key. Skipping
       ! over deleted slots ensures that a key is not added twice, in case it is
       ! not at its 'first' hash index, and some keys in between have been deleted.
       do step = 1, h%n_buckets
          if (bucket_empty(h, i) .or. &
               (.not. bucket_deleted(h, i) .and. h%keys(i) == key)) exit
          if (bucket_deleted(h, i)) i_deleted = i
          i = next_index(h, i, step)
       end do

       if (bucket_empty(h, i) .and. i_deleted /= -1) then
          ! Use deleted location
          i = i_deleted
       end if
    end if

    if (bucket_empty(h, i)) then
       h%n_occupied    = h%n_occupied + 1
       h%n_keys_stored = h%n_keys_stored + 1
       h%keys(i)       = key
       call set_bucket_filled(h, i)
    else if (bucket_deleted(h, i)) then
       h%n_keys_stored = h%n_keys_stored + 1
       h%keys(i)       = key
       call set_bucket_filled(h, i)
    end if
    ! If key is already present, do nothing

  end function ffh_store_key

  !> Resize a hash table
  subroutine ffh_resize(h, new_n_buckets, status)
    type(ffh_t), intent(inout) :: h
    integer, intent(in)        :: new_n_buckets
    integer, intent(out)       :: status
    integer                    :: n_new, i, j, step
    type(ffh_t)                :: hnew

    ! Make sure n_new is a power of two, and at least 4
    n_new = 4
    do while (n_new < new_n_buckets)
       n_new = 2 * n_new
    end do

    if (h%n_keys_stored >= nint(n_new * h%max_load_factor)) then
       ! Requested size is too small
       status = ffh_fail
       return
    end if

    ! Expand or shrink table
#ifdef VAL_TYPE
    allocate(hnew%flags(0:n_new-1), hnew%keys(0:n_new-1), &
         hnew%vals(0:n_new-1), stat=status)
#else
    allocate(hnew%flags(0:n_new-1), hnew%keys(0:n_new-1), stat=status)
#endif
    if (status /= 0) then
       status = ffh_fail
       return
    end if

    hnew%flags(:)        = achar(0)
    hnew%n_keys_stored   = h%n_keys_stored
    hnew%n_occupied      = h%n_keys_stored
    hnew%n_buckets       = n_new
    hnew%max_load_factor = h%max_load_factor
    hnew%n_occupied_max  = nint(n_new * hnew%max_load_factor)
    hnew%hash_mask       = n_new - 1

    do j = 0, h%n_buckets-1
       if (ffh_valid_index(h, j)) then
          ! Find a new index
          i = hash_index(hnew, h%keys(j))

          do step = 1, hnew%n_buckets
             if (bucket_empty(hnew, i)) exit
             i = next_index(hnew, i, step)
          end do
#ifdef VAL_TYPE
          hnew%vals(i) = h%vals(j)
#endif
          hnew%keys(i) = h%keys(j)
          call set_bucket_filled(hnew, i)
       end if
    end do

    h      = hnew
    status = ffh_success
  end subroutine ffh_resize

  !> Delete entry for given key
  subroutine ffh_delete_key(h, key, status)
    type(ffh_t), intent(inout) :: h
    KEY_TYPE, intent(in)       :: key
    integer, intent(out)       :: status
    integer                    :: ix

    ix = ffh_get_index(h, key)
    if (ix /= ffh_fail) then
       call set_bucket_deleted(h, ix)
       h%n_keys_stored = h%n_keys_stored - 1
       status = ffh_success
    else
       status = ffh_fail
    end if
  end subroutine ffh_delete_key

  !> Delete entry at index
  subroutine ffh_delete_index(h, ix, status)
    type(ffh_t), intent(inout) :: h
    integer, intent(in)        :: ix
    integer, intent(out)       :: status

    if (ix < lbound(h%keys, 1) .or. ix > ubound(h%keys, 1)) then
       status = ffh_fail
    else if (.not. ffh_valid_index(h, ix)) then
       status = ffh_fail
    else
       call set_bucket_deleted(h, ix)
       h%n_keys_stored = h%n_keys_stored - 1
       status = ffh_success
    end if
  end subroutine ffh_delete_index

  pure logical function bucket_empty(h, i)
    type(ffh_t), intent(in) :: h
    integer, intent(in)       :: i
    bucket_empty = (iand(iachar(h%flags(i)), 1) == 0)
  end function bucket_empty

  pure logical function bucket_deleted(h, i)
    type(ffh_t), intent(in) :: h
    integer, intent(in)       :: i
    bucket_deleted = (iand(iachar(h%flags(i)), 2) /= 0)
  end function bucket_deleted

  !> Check if index is used and not deleted
  pure logical function ffh_valid_index(h, i)
    type(ffh_t), intent(in) :: h
    integer, intent(in)       :: i
    ffh_valid_index = (iachar(h%flags(i)) == 1)
  end function ffh_valid_index

  pure subroutine set_bucket_filled(h, i)
    type(ffh_t), intent(inout) :: h
    integer, intent(in)          :: i
    h%flags(i) = achar(1)
  end subroutine set_bucket_filled

  pure subroutine set_bucket_deleted(h, i)
    type(ffh_t), intent(inout) :: h
    integer, intent(in)          :: i
    h%flags(i) = achar(ior(iachar(h%flags(i)), 2))
  end subroutine set_bucket_deleted

  !> Compute index for given key
  pure integer function hash_index(h, key) result(i)
    type(ffh_t), intent(in) :: h
    KEY_TYPE, intent(in)    :: key
    i = iand(hash_function(key), h%hash_mask)
  end function hash_index

  !> Compute next index inside a loop
  pure integer function next_index(h, i_prev, step)
    type(ffh_t), intent(in) :: h
    integer, intent(in)       :: i_prev
    integer, intent(in)       :: step
    next_index = iand(i_prev + step, h%hash_mask)
  end function next_index

#ifndef CUSTOM_HASH_FUNCTION
  pure function hash_function(key) result(hash)
    KEY_TYPE, intent(in)   :: key
    integer                :: hash
    integer, parameter     :: n_bytes = ceiling(storage_size(key)*0.125d0)
    integer, parameter     :: seed    = 42
    character(len=n_bytes) :: buf

    call MurmurHash3_x86_32(transfer(key, buf), n_bytes, seed, hash)
  end function hash_function

  pure integer(int32) function rotl32(x, r)
    use iso_fortran_env
    integer(int32), intent(in) :: x
    integer(int8), intent(in)  :: r
    rotl32 = ior(shiftl(x, r), shiftr(x, (32 - r)))
  end function rotl32

  pure integer(int64) function rotl64(x, r)
    use iso_fortran_env
    integer(int64), intent(in) :: x
    integer(int8), intent(in)  :: r
    rotl64 = ior(shiftl(x, r), shiftr(x, (64 - r)))
  end function rotl64

  ! Finalization mix - force all bits of a hash block to avalanche
  pure integer(int32) function fmix32(h_in) result(h)
    use iso_fortran_env
    integer(int32), intent(in) :: h_in
    h = h_in
    h = ieor(h, shiftr(h, 16))
    h = h * (-2048144789) !0x85ebca6b
    h = ieor(h, shiftr(h, 13))
    h = h * (-1028477387) !0xc2b2ae35
    h = ieor(h, shiftr(h, 16))
  end function fmix32

  pure subroutine MurmurHash3_x86_32(key, klen, seed, hash)
    use iso_fortran_env
    integer, intent(in)             :: klen
    character(len=klen), intent(in) :: key
    integer(int32), intent(in)      :: seed
    integer(int32), intent(out)     :: hash
    integer                         :: i, i0, n, nblocks
    integer(int32)                  :: h1, k1
    integer(int32), parameter       :: c1        = -862048943 ! 0xcc9e2d51
    integer(int32), parameter       :: c2        = 461845907  !0x1b873593
    integer, parameter              :: shifts(3) = [0, 8, 16]

    h1      = seed
    nblocks = shiftr(klen, 2)    ! nblocks/4

    ! body
    do i = 1, nblocks
       k1 = transfer(key(i*4-3:i*4), k1)

       k1 = k1 * c1
       k1 = rotl32(k1,15_int8)
       k1 = k1 * c2

       h1 = ieor(h1, k1)
       h1 = rotl32(h1,13_int8)
       h1 = h1 * 5 - 430675100  ! 0xe6546b64
    end do

    ! tail
    k1 = 0
    i  = iand(klen, 3)
    i0 = 4 * nblocks

    do n = i, 1, -1
       k1 = ieor(k1, shiftl(iachar(key(i0+n:i0+n)), shifts(n)))
    end do

    ! Check if the above loop was executed
    if (i >= 1) then
       k1 = k1 * c1
       k1 = rotl32(k1,15_int8)
       k1 = k1 * c2
       h1 = ieor(h1, k1)
    end if

    ! finalization
    h1 = ieor(h1, klen)
    h1 = fmix32(h1)
    hash = h1
  end subroutine MurmurHash3_x86_32
#endif
