#define ncols 8

#ifdef ONEoTWO
#define POWER_VAL 0.5
#define POWER_NOM 1
#define POWER_DENOM 2
#else
#define POWER_VAL 1.5
#define POWER_NOM 3
#define POWER_DENOM 2
#endif

#ifdef CHECK
#ifndef THREEHALVES_A
#define THREEHALVES_A
#endif
#ifndef THREEHALVES_B
#define THREEHALVES_B
#endif
#ifndef THREEHALVES_C
#define THREEHALVES_C
#endif
#ifndef THREEHALVES_D
#define THREEHALVES_D
#endif
#ifndef THREEHALVES_E
#define THREEHALVES_E
#endif
#ifndef THREEHALVES_F
#define THREEHALVES_F
#endif
#ifndef THREEHALVES_G
#define THREEHALVES_G
#endif
#endif

program test_three_halves
  use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
  implicit none

  integer :: i, j
#ifdef CHECK
  integer, parameter :: n=1, np=2000000
#else
  integer, parameter :: n=200, np=2000000
#endif


  real(kind=REAL64), dimension(:,:), allocatable :: array
  allocate(array(np,ncols))

  call fill_sub(array)
  do i=1,n
    call test_sub(array)
    call dummy_sub(array, i) ! dummy_sub prevents the compiler from rewriting test_sub
  enddo
#ifdef CHECK
  do j=2,ncols
    print *, j, verify_sub(array, j)
  enddo
#endif

contains

  subroutine test_sub(arr)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer :: i, imax
    imax = ubound(arr, 1)

#ifdef THREEHALVES_A
    do concurrent(i=1:imax)
      arr(i,2) = arr(i,1)**(-POWER_VAL)
    enddo
#endif
#ifdef THREEHALVES_B
    do i=1,imax
      arr(i,3) = 1.0/arr(i,1)**(POWER_VAL)
    enddo
#endif
#ifdef THREEHALVES_C
    do i=1,imax
      arr(i,4) = 1.0/sqrt(arr(i,1)**POWER_NOM)
    enddo
#endif
#ifdef THREEHALVES_D
    do i=1,imax
      arr(i,5) = 1.0/sqrt(arr(i,1))**POWER_NOM
    enddo
#endif
#ifdef THREEHALVES_E
    do i=1,imax
      arr(i,6) = (1.0/sqrt(arr(i,1)))**POWER_NOM
    enddo
#endif
#ifdef THREEHALVES_F
    do i=1,imax
      arr(i,7) = sqrt(arr(i,1))**(-POWER_NOM)
    enddo
#endif
#ifdef THREEHALVES_G
    arr(:,8) = arr(:,1)**(-POWER_VAL)
#endif

  end subroutine test_sub

  subroutine dummy_sub(arr, i)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer, intent(in) :: i
    integer :: imax, jmax
    imax = ubound(arr, 1)
    jmax = ubound(arr, 2)

    if(i*sin(real(i)) - arr(imax-1, jmax-1) == 0.1234) then
      arr(:,:) = arr(:,:) * 1.1
      print *, arr(1,int(arr(1,3000)/arr(1,2000)))
    endif

  end subroutine dummy_sub

  subroutine fill_sub(arr)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer :: i, imax

    imax = ubound(arr, 1)
    do i=1,imax
      arr(i, 1) = real(i, kind=REAL64) * 0.001 + 0.1/3.
    enddo
  end subroutine fill_sub

  function verify_sub(arr, col)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer, intent(in) :: col
    real(kind=REAL64) :: verify_sub
    integer :: i, imax
    real(kind=REAL64) :: elem_diff
    verify_sub = -1.0

    imax = ubound(arr, 1)
    do i=1,imax
      elem_diff = abs(arr(i, col) - arr(i, 2))
      if (elem_diff > verify_sub) then
        verify_sub = elem_diff
      endif
    enddo
  end function verify_sub

end program test_three_halves
