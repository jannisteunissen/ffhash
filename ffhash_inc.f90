  ! This file should be included in a module ... end module block
  ! For example:
  ! module m_ffhash
  ! implicit none
  ! #define FFH_KEY_TYPE integer                  !use this for non-string keys
  ! #define FFH_STRING_KEY_TYPE character(len=15) !use this for string keys
                                                  !(FFH_KEY_TYPE = FFH_STRING_KEY_TYPE 
                                                  !will be set in ffhash_inc.f90)
                                                  !
  ! #define FFH_VAL_TYPE integer (optional)       !use this for non-string values
  ! #define FFH_STRING_VAL_TYPE character(len=30) !use this for string values 
                                                  !(FFH_VAL_TYPE = FFH_STRING_VAL_TYPE 
                                                  !will be set in ffhash_inc.f90)
  ! #include "ffhash_inc.f90"
  ! end module m_ffhash

  ! Defining FFH's integer kind 
#ifdef FFH_ENABLE_INT64
#define FFH_INT_KIND int64 
#else
#define FFH_INT_KIND int32 
#endif

  ! Special handling of strings (which can be shortened)
#ifdef FFH_STRING_KEY_TYPE
#define FFH_KEY_TYPE FFH_STRING_KEY_TYPE
#define FFH_KEY_ARG character(len=*)
#else
#define FFH_KEY_ARG FFH_KEY_TYPE
#endif

#ifdef FFH_STRING_VAL_TYPE
#define FFH_VAL_TYPE FFH_STRING_VAL_TYPE
#define FFH_VAL_ARG character(len=*)
#else
#define FFH_VAL_ARG FFH_VAL_TYPE
#endif



  private

  !> Type storing the hash table
  type, public :: ffh_t
     !> Number of buckets in hash table
     integer(FFH_INT_KIND)      :: n_buckets       = 0
     !> Number of keys stored in hash table
     integer(FFH_INT_KIND)      :: n_keys_stored   = 0
     !> Number of keys stored or deleted
     integer(FFH_INT_KIND)      :: n_occupied      = 0
     !> Maximum number of occupied buckets
     integer(FFH_INT_KIND)      :: n_occupied_max  = 0
     !> Mask to convert hash to index
     integer(FFH_INT_KIND)      :: hash_mask       = 0
     !> Maximum load factor for the hash table
     double precision           :: max_load_factor = 0.7d0
     !> Flags indicating whether buckets are empty or deleted
     character, allocatable     :: flags(:)
     !> Keys of the hash table
     FFH_KEY_TYPE, allocatable  :: keys(:)
#ifdef FFH_VAL_TYPE
     !> Values stored for the keys
     FFH_VAL_TYPE, allocatable  :: vals(:)
#endif
   contains
     !> Get index of a key
     procedure, non_overridable :: get_index
     !> Check whether a valid key is present at index
     procedure, non_overridable :: valid_index
     !> Store a new key
     procedure, non_overridable :: store_key
     !> Delete a key
     procedure, non_overridable :: delete_key
     !> Delete a key (can perform error stop)
     procedure, non_overridable :: udelete_key
     !> Delete key at an index
     procedure, non_overridable :: delete_index
     !> Delete key at an index (can perform error stop)
     procedure, non_overridable :: udelete_index
     !> Resize the hash table
     procedure, non_overridable :: resize
     !> Reset the hash table to initial empty state
     procedure, non_overridable :: reset
#ifdef FFH_VAL_TYPE
     !> Store a key-value pair
     procedure, non_overridable :: store_value
     !> Store a key-value pair (can perform error stop)
     procedure, non_overridable :: ustore_value
     !> Get value for a key
     procedure, non_overridable :: get_value
     !> Get value for a key (can perform error stop)
     procedure, non_overridable :: uget_value
     !> Function to get value for a key (can perform error stop)
     procedure, non_overridable :: fget_value
     !> Function to get value for a key or a dummy if not found
     procedure, non_overridable :: fget_value_or
#endif
     !> Hash function
     procedure, non_overridable, nopass :: hash_function
  end type ffh_t

contains

  !> Get index corresponding to a key. If the key is not found, return -1.
  elemental pure function get_index(h, key) result(ix)
    class(ffh_t), intent(in) :: h
    FFH_KEY_ARG, intent(in)  :: key
    integer(FFH_INT_KIND)    :: ix, i, step

    ix = -1
    i  = hash_index(h, key)

    do step = 1, h%n_buckets
       ! Exit when an empty bucket or the key is found
       if (bucket_empty(h, i)) then
          ! Key not found
          exit
       else if (h%valid_index(i)) then
          if (keys_equal(h%keys(i), key)) then
             ! Key found
             ix = i
             exit
          end if
       end if
       i = next_index(h, i, step)
    end do
  end function get_index

#ifdef FFH_VAL_TYPE
  !> Get the value corresponding to a key
  pure subroutine get_value(h, key, val, status)
    class(ffh_t), intent(in)           :: h
    FFH_KEY_ARG, intent(in)            :: key
    FFH_VAL_ARG, intent(inout)         :: val
    integer(FFH_INT_KIND), intent(out) :: status

    status = h%get_index(key)
    if (status >= 0) val = h%vals(status)
  end subroutine get_value

  !> Get the value corresponding to a key
  subroutine uget_value(h, key, val)
    class(ffh_t), intent(in)   :: h
    FFH_KEY_ARG, intent(in)    :: key
    FFH_VAL_ARG, intent(inout) :: val
    integer(FFH_INT_KIND)      :: status
    call get_value(h, key, val, status)
    if (status < 0) error stop "Cannot get value"
  end subroutine uget_value

  !> Get the value corresponding to a key
  function fget_value(h, key) result(val)
    class(ffh_t), intent(in) :: h
    FFH_KEY_ARG, intent(in)  :: key
    FFH_VAL_TYPE             :: val
    integer(FFH_INT_KIND)    :: status
    call get_value(h, key, val, status)
    if (status < 0) error stop "Cannot get value"
  end function fget_value

  !> Get the value corresponding to a key
  elemental pure function fget_value_or(h, key, not_found) result(val)
    class(ffh_t), intent(in) :: h
    FFH_KEY_ARG, intent(in)  :: key
    FFH_VAL_ARG, intent(in)  :: not_found
    FFH_VAL_TYPE             :: val
    integer(FFH_INT_KIND)    :: status
    call get_value(h, key, val, status)
    if (status < 0) val = not_found
  end function fget_value_or

  !> Store the value corresponding to a key
  subroutine store_value(h, key, val, ix, existing_key_is_error)
    class(ffh_t), intent(inout)        :: h
    FFH_KEY_ARG, intent(in)            :: key
    FFH_VAL_ARG, intent(in)            :: val
    integer(FFH_INT_KIND), intent(out) :: ix !< Index (or -1 / -2)
    logical, optional, intent(in)      :: existing_key_is_error

    call h%store_key(key, ix, existing_key_is_error)
    if (ix >= 0) h%vals(ix) = val
  end subroutine store_value

  subroutine ustore_value(h, key, val)
    class(ffh_t), intent(inout) :: h
    FFH_KEY_ARG, intent(in)     :: key
    FFH_VAL_ARG, intent(in)     :: val
    integer(FFH_INT_KIND)       :: ix
    call store_value(h, key, val, ix)
    if (ix < 0) error stop "Cannot store value"
  end subroutine ustore_value
#endif

  !> Store key in the table, and return index. A negative index is returned in
  !> case of an error. If resizing fails, -1 is returned. If the key was
  !> already present, and existing_key_is_error (an optional flag to decide 
  !> whether storing an already existing key should throw an error) is true, 
  !> the returned index is set to -2.
  subroutine store_key(h, key, i, existing_key_is_error)
    class(ffh_t), intent(inout)        :: h
    FFH_KEY_ARG, intent(in)            :: key
    integer(FFH_INT_KIND), intent(out) :: i
    logical, optional, intent(in)      :: existing_key_is_error
    logical                            :: error_if_exists
    integer(FFH_INT_KIND)              :: i_deleted, step, status

    ! Create a local copy of "existing_key_is_error"
    if(present(existing_key_is_error)) then
       error_if_exists = existing_key_is_error
    else
       error_if_exists = .false.
    endif

    i = -1

    if (h%n_occupied >= h%n_occupied_max) then
       if (h%n_keys_stored <= h%n_occupied_max/2) then
          ! Enough free space, but need to clean up the table
          call h%resize(h%n_buckets, status)
          if (status /= 0) return
       else
          ! Increase table size
          call h%resize(2*h%n_buckets, status)
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
          if (bucket_empty(h, i)) exit

          ! Check if key is already present
          if (.not. bucket_deleted(h, i) .and. &
               keys_equal(h%keys(i), key)) then
             if (error_if_exists) then
                ! Throw error
                i = -2
                return
             else
                exit
             end if
          end if

          if (bucket_deleted(h, i)) i_deleted = i
          i = next_index(h, i, step)
       end do

       if (bucket_empty(h, i) .and. i_deleted /= -1) then
          ! Use deleted location. By taking the last one, the deleted sequence
          ! is shrunk from the end.
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

  end subroutine store_key

  !> Resize a hash table
  pure subroutine resize(h, new_n_buckets, status)
    class(ffh_t), intent(inout)        :: h
    integer(FFH_INT_KIND), intent(in)  :: new_n_buckets
    integer(FFH_INT_KIND), intent(out) :: status
    integer(FFH_INT_KIND)              :: n_new, i, j, step
    type(ffh_t)                        :: hnew

    ! Make sure n_new is a power of two, and at least 4
    n_new = 4
    do while (n_new < new_n_buckets)
       n_new = 2 * n_new
    end do

    if (h%n_keys_stored >= nint(n_new * h%max_load_factor)) then
       ! Requested size is too small
       status = -1
       return
    end if

    ! Expand or shrink table
#ifdef FFH_VAL_TYPE
    allocate(hnew%flags(0:n_new-1), hnew%keys(0:n_new-1), &
         hnew%vals(0:n_new-1), stat=status)
#else
    allocate(hnew%flags(0:n_new-1), hnew%keys(0:n_new-1), stat=status)
#endif
    if (status /= 0) then
       status = -1
       return
    end if

    hnew%flags(:)        = achar(0)
    hnew%n_buckets       = n_new
    hnew%n_keys_stored   = h%n_keys_stored
    hnew%n_occupied      = h%n_keys_stored
    hnew%n_occupied_max  = nint(n_new * hnew%max_load_factor)
    hnew%hash_mask       = n_new - 1
    hnew%max_load_factor = h%max_load_factor

    do j = 0, h%n_buckets-1
       if (h%valid_index(j)) then
          ! Find a new index
          i = hash_index(hnew, h%keys(j))

          do step = 1, hnew%n_buckets
             if (bucket_empty(hnew, i)) exit
             i = next_index(hnew, i, step)
          end do
#ifdef FFH_VAL_TYPE
          hnew%vals(i) = h%vals(j)
#endif
          hnew%keys(i) = h%keys(j)
          call set_bucket_filled(hnew, i)
       end if
    end do

    h%n_buckets       = hnew%n_buckets
    h%n_keys_stored   = hnew%n_keys_stored
    h%n_occupied      = hnew%n_occupied
    h%n_occupied_max  = hnew%n_occupied_max
    h%hash_mask       = hnew%hash_mask
    h%max_load_factor = hnew%max_load_factor

    call move_alloc(hnew%flags, h%flags)
    call move_alloc(hnew%keys, h%keys)
#ifdef FFH_VAL_TYPE
    call move_alloc(hnew%vals, h%vals)
#endif

    status = 0
  end subroutine resize

  !> Delete entry for given key
  pure subroutine delete_key(h, key, status)
    class(ffh_t), intent(inout)        :: h
    FFH_KEY_ARG, intent(in)            :: key
    integer(FFH_INT_KIND), intent(out) :: status
    integer(FFH_INT_KIND)              :: ix

    ix = h%get_index(key)
    if (ix >= 0) then
       call set_bucket_deleted(h, ix)
       h%n_keys_stored = h%n_keys_stored - 1
       status = 0
    else
       status = -1
    end if
  end subroutine delete_key

  !> Delete entry for given key
  subroutine udelete_key(h, key)
    class(ffh_t), intent(inout) :: h
    FFH_KEY_ARG, intent(in)     :: key
    integer(FFH_INT_KIND)       :: status
    call h%delete_key(key, status)
    if (status < 0) error stop "Cannot delete key"
  end subroutine udelete_key

  !> Delete entry at index. A negative status indicates an error.
  pure subroutine delete_index(h, ix, status)
    class(ffh_t), intent(inout)        :: h
    integer(FFH_INT_KIND), intent(in)  :: ix
    integer(FFH_INT_KIND), intent(out) :: status

    if (ix < lbound(h%keys, 1) .or. ix > ubound(h%keys, 1)) then
       status = -1
    else if (.not. h%valid_index(ix)) then
       status = -1
    else
       call set_bucket_deleted(h, ix)
       h%n_keys_stored = h%n_keys_stored - 1
       status = 0
    end if
  end subroutine delete_index

  !> Delete entry at index
  subroutine udelete_index(h, ix)
    class(ffh_t), intent(inout)        :: h
    integer(FFH_INT_KIND), intent(in)  :: ix
    integer(FFH_INT_KIND)              :: status
    call h%delete_index(ix, status)
    if (status < 0) error stop "Cannot delete key"
  end subroutine udelete_index

  !> Reset the hash table to initial empty state
  subroutine reset(h)
    class(ffh_t), intent(inout) :: h

    h%n_buckets       = 0
    h%n_keys_stored   = 0
    h%n_occupied      = 0
    h%n_occupied_max  = 0
    h%hash_mask       = 0

    if (h%n_buckets > 0) then
       deallocate(h%flags)
       deallocate(h%keys)
#ifdef FFH_VAL_TYPE
       deallocate(h%vals)
#endif
    end if
  end subroutine reset

  pure logical function bucket_empty(h, i)
    type(ffh_t), intent(in)           :: h
    integer(FFH_INT_KIND), intent(in) :: i
    bucket_empty = (iand(iachar(h%flags(i)), 1) == 0)
  end function bucket_empty

  pure logical function bucket_deleted(h, i)
    type(ffh_t), intent(in)           :: h
    integer(FFH_INT_KIND), intent(in) :: i
    bucket_deleted = (iand(iachar(h%flags(i)), 2) /= 0)
  end function bucket_deleted

  !> Check if index is used and not deleted
  pure logical function valid_index(h, i)
    class(ffh_t), intent(in)          :: h
    integer(FFH_INT_KIND), intent(in) :: i
    valid_index = (iachar(h%flags(i)) == 1)
  end function valid_index

  pure subroutine set_bucket_filled(h, i)
    type(ffh_t), intent(inout)        :: h
    integer(FFH_INT_KIND), intent(in) :: i
    h%flags(i) = achar(1)
  end subroutine set_bucket_filled

  pure subroutine set_bucket_deleted(h, i)
    type(ffh_t), intent(inout)        :: h
    integer(FFH_INT_KIND), intent(in) :: i
    h%flags(i) = achar(ior(iachar(h%flags(i)), 2))
  end subroutine set_bucket_deleted

  !> Compute index for given key
  pure integer(FFH_INT_KIND) function hash_index(h, key) result(i)
    type(ffh_t), intent(in) :: h
    FFH_KEY_ARG, intent(in) :: key
    i = iand(h%hash_function(key), h%hash_mask)
  end function hash_index

  !> Compute next index inside a loop
  pure integer(FFH_INT_KIND) function next_index(h, i_prev, step)
    type(ffh_t), intent(in)           :: h
    integer(FFH_INT_KIND), intent(in) :: i_prev
    integer(FFH_INT_KIND), intent(in) :: step
    next_index = iand(i_prev + step, h%hash_mask)
  end function next_index

#ifndef FFH_CUSTOM_KEYS_EQUAL
  pure logical function keys_equal(a, b)
    FFH_KEY_ARG, intent(in) :: a, b
    keys_equal = (a == b)
  end function keys_equal
#endif

#ifndef FFH_CUSTOM_HASH_FUNCTION
  pure function hash_function(key) result(hash)
    FFH_KEY_ARG, intent(in)   :: key
    integer(FFH_INT_KIND)     :: hash
    integer(int32), parameter :: seed = 42

#ifdef FFH_CUSTOM_CONVERT_KEY
    hash = default_hash_core(convert_key(key), seed)
#elif defined(FFH_STRING_KEY_TYPE)
    hash = default_hash_core(trim(key), seed)
#else
    integer, parameter        :: n_bytes = ceiling(storage_size(key)*0.125d0)
    character(len=n_bytes)    :: buf
    buf = transfer(key, buf)
    hash = default_hash_core(buf, seed)
#endif
  end function hash_function

!helper function managing the int64-based hashing
  pure function default_hash_core(buf, seed) result(hash)
    character(len=*), intent(in) :: buf
    integer(int32),   intent(in) :: seed
    integer(FFH_INT_KIND)        :: hash
#ifdef FFH_ENABLE_INT64
    integer(int32) :: hash128(4)
    integer(int64) :: h1, h2
    call MurmurHash3_x64_128(buf, len(buf), seed, hash128)
    h1 = transfer(hash128(1:2), h1)
    h2 = transfer(hash128(3:4), h2)
    hash = ieor(h1, h2)
#else
    call MurmurHash3_x86_32(buf, len(buf), seed, hash)
#endif
  end function default_hash_core

#ifdef FFH_ENABLE_INT64

  pure subroutine MurmurHash3_x64_128(key, klen, seed, hash)
    integer, intent(in)             :: klen
    character(len=klen), intent(in) :: key
    integer(int32), intent(in)      :: seed
    integer(int32), intent(out)     :: hash(4)
    integer                         :: i, i0, n, nblocks
    integer(int64)                  :: h1, h2, k1, k2
    ! 0x87c37b91114253d5
    integer(int64), parameter       :: c1         = -8663945395140668459_int64
    ! 0x4cf5ad432745937f
    integer(int64), parameter       :: c2         = 5545529020109919103_int64
    integer, parameter              :: shifts(15) = [(i*8, i=0,7), (i*8, i=0,6)]

    h1      = seed
    h2      = seed
    nblocks = shiftr(klen, 4)    ! nblocks / 16

    ! body
    do i = 1, nblocks
       k1 = transfer(key(i*16-15:i*16-8), k1)
       k2 = transfer(key(i*16-7:i*16), k2)

       k1 = k1 * c1
       k1 = rotl64(k1,31_int64)
       k1 = k1 * c2

       h1 = ieor(h1, k1)
       h1 = rotl64(h1,27_int64)
       h1 = h1 + h2
       h1 = h1 * 5 + 1390208809_int64 ! 0x52dce729

       k2 = k2 * c2
       k2 = rotl64(k2,33_int64)
       k2 = k2 * c1

       h2 = ieor(h2, k2)
       h2 = rotl64(h2,31_int64)
       h2 = h1 + h2
       h2 = h2 * 5 + 944331445 ! 0x38495ab5
    end do

    ! tail
    k1 = 0
    k2 = 0
    i  = iand(klen, 15)
    i0 = 16 * nblocks

    do n = i, 9, -1
       k2 = ieor(k2, shiftl(iachar(key(i0+n:i0+n), int64), shifts(n)))
    end do

    ! Check if the above loop was executed
    if (i >= 9) then
       k2 = k2 * c2
       k2  = rotl64(k2,33_int64)
       k2 = k2 * c1
       h2 = ieor(h2, k2)
    end if

    do n = min(i, 8), 1, -1
       k1 = ieor(k1, shiftl(iachar(key(i0+n:i0+n), int64), shifts(n)))
    end do

    ! Check if the above loop was executed
    if (i >= 1) then
       k1 = k1 * c1
       k1 = rotl64(k1,31_int64)
       k1 = k1 * c2
       h1 = ieor(h1, k1)
    end if

    ! finalization
    h1 = ieor(h1, int(klen, int64))
    h2 = ieor(h2, int(klen, int64))

    h1 = h1 + h2
    h2 = h2 + h1

    h1 = fmix64(h1)
    h2 = fmix64(h2)

    h1 = h1 + h2
    h2 = h2 + h1

    hash = transfer([h1, h2], hash)
  end subroutine MurmurHash3_x64_128

  pure integer(int64) function rotl64(x, r)
    integer(int64), intent(in) :: x
    integer(int64), intent(in)  :: r
    rotl64 = ior(shiftl(x, r), shiftr(x, (64 - r)))
  end function rotl64

  pure integer(int64) function fmix64(k_in) result(k)
    integer(int64), intent(in) :: k_in
    k = k_in
    k = ieor(k, shiftr(k, 33))
    k = k * (-49064778989728563_int64) !0xff51afd7ed558ccd
    k = ieor(k, shiftr(k, 33))
    k = k * (-4265267296055464877_int64) !0xc4ceb9fe1a85ec53
    k = ieor(k, shiftr(k, 33))
  end function fmix64

#else

  pure subroutine MurmurHash3_x86_32(key, klen, seed, hash)
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
       k1 = rotl32(k1,15_int64)
       k1 = k1 * c2

       h1 = ieor(h1, k1)
       h1 = rotl32(h1,13_int64)
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
       k1 = rotl32(k1,15_int64)
       k1 = k1 * c2
       h1 = ieor(h1, k1)
    end if

    ! finalization
    h1 = ieor(h1, klen)
    h1 = fmix32(h1)
    hash = h1
  end subroutine MurmurHash3_x86_32

  pure integer(int32) function rotl32(x, r)
    integer(int32), intent(in) :: x
    integer(int64), intent(in)  :: r
    rotl32 = ior(shiftl(x, r), shiftr(x, (32 - r)))
  end function rotl32

  ! Finalization mix - force all bits of a hash block to avalanche
  pure integer(int32) function fmix32(h_in) result(h)
    integer(int32), intent(in) :: h_in
    h = h_in
    h = ieor(h, shiftr(h, 16))
    h = h * (-2048144789) !0x85ebca6b
    h = ieor(h, shiftr(h, 13))
    h = h * (-1028477387) !0xc2b2ae35
    h = ieor(h, shiftr(h, 16))
  end function fmix32
#endif
#endif

  ! So that this file can be included multiple times
#undef FFH_ENABLE_INT64
#undef FFH_INT_KIND
#undef FFH_KEY_TYPE
#undef FFH_STRING_KEY_TYPE
#undef FFH_KEY_ARG
#undef FFH_VAL_TYPE
#undef FFH_STRING_VAL_TYPE
#undef FFH_VAL_ARG
#undef FFH_CUSTOM_HASH_FUNCTION
#undef FFH_CUSTOM_KEYS_EQUAL
