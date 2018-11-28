module mapgen

  ! type, public :: 2DArray
  !    integer:: row, col
  !    integer, dimension(:) :: array

  ! end type 2DArray


contains
  function generate_random(x,y, pulse_count, pulse_value) result(result)
    integer:: x,y, pulse_count, pulse_value
    integer, dimension(x,y) :: result
    integer, dimension(3,3) :: kernel

    result = zeros(x,y)

    do n=1,pulse_count
       result(randint(1,x), randint(1, y)) = pulse_value
    end do

    kernel = reshape((/0, 1, 0, 1, 4, 1, 0, 1, 0/), (/3,3/))

    call convolute(result, kernel, .true., .true., 8)
    call convolute(result, kernel, .true., .true., 8)

  end function

  function zeros(x,y) result(result)
    integer:: x,y
    integer, dimension(x,y) :: result

    do i_x=1,x
       do i_y=1,y
          result(i_x, i_y) = 0
       end do
    end do
  end function

  subroutine print_map(map)
    integer, dimension(:,:) :: map
    integer, dimension(2) :: the_shape

    the_shape = shape(map)
    print*, "Shape :", the_shape

    do i=1,the_shape(1)
       print*, map(i,:)
    end do

  end subroutine

  integer function randint(min, max)
    ! max not included
    integer,intent(in):: min, max
    real:: rand
    call random_number(rand)

    randint = min + floor(rand * max)

  end function

  subroutine convolute(array, kernel, horizontal_mirror, vertical_mirror, factor)
    integer, dimension(:,:):: array, kernel
    logical :: horizontal_mirror, vertical_mirror
    integer, dimension(2) :: array_shape, kernel_shape
    integer, pointer :: original_array(:,:)
    integer :: factor

    allocate(original_array, source=array)
    array_shape = shape(array)

    do kernel_center_row = 1, array_shape(1)
       do kernel_center_col = 1, array_shape(2)
          array(kernel_center_row, kernel_center_col) = kernel_sum(kernel_center_row, &
               kernel_center_col, original_array, kernel, horizontal_mirror, vertical_mirror) / factor
       end do
    end do

    deallocate(original_array)
  end subroutine

  integer function kernel_sum(kernel_center_row, kernel_center_col, array, kernel, horizontal_mirror, vertical_mirror)
    integer :: kernel_center_row, kernel_center_col
    integer, dimension(:,:) :: array, kernel
    logical :: horizontal_mirror, vertical_mirror
    integer, dimension(2):: kernel_shape, array_shape
    integer :: tile_row, tile_col, row_offset, column_offset

    array_shape = shape(array)
    kernel_shape = shape(kernel)

    kernel_sum = 0
    do kernel_row=1,kernel_shape(1)
       do kernel_column=1,kernel_shape(2)
          row_offset = kernel_row - ceiling(kernel_shape(1) / 2.0)
          column_offset = kernel_column - ceiling(kernel_shape(2) / 2.0)

          tile_row = offset_tile(kernel_center_row, row_offset, vertical_mirror, array_shape(1))
          tile_col = offset_tile(kernel_center_col, column_offset, horizontal_mirror, array_shape(2))

          kernel_sum = kernel_sum + kernel(kernel_row, kernel_column)*array(tile_row, tile_col)
       end do
    end do
  end function

  integer function offset_tile(base, offset, mirror, max_value)
    integer :: base, offset, max_value
    logical :: mirror

    offset_tile = base + offset

    if (offset_tile > max_value) then
       if (mirror) then
          offset_tile = 2 * max_value - offset_tile
       else
          offset_tile = offset_tile - max_value
       end if
    end if

    if (offset_tile < 1) then
       if (mirror) then
          offset_tile = 2 - offset_tile
       else
          offset_tile = offset_tile + max_value
       end if
       end if
     end function


   end module

