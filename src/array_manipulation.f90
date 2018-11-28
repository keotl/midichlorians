module array_manipulation
  integer :: VERTICAL = 0
  integer :: HORIZONTAL = 1

contains
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

  function concatenate(array1, array2, axis) result(result)
    integer, dimension(:,:) :: array1, array2
    integer :: axis
    integer, allocatable :: result(:,:)
    integer :: rows, columns
    integer, dimension(2) :: shape1, shape2
    integer :: copy_cursor
    copy_cursor = 1

    shape1 = shape(array1)
    shape2 = shape(array2)

    if (axis == VERTICAL) then
       rows = shape1(1) + shape2(1)
       columns = shape1(2)
    else
       rows = shape1(1)
       columns = shape1(2) + shape2(2)
    end if

    allocate(result(rows, columns))

    if (axis == VERTICAL) then
       do n=1,shape1(1)
          result(copy_cursor, :) = array1(n, :)
          copy_cursor = copy_cursor + 1
       end do
       do n=1, shape2(1)
          result(copy_cursor, :) = array2(n, :)
          copy_cursor = copy_cursor + 1
       end do
    else
       do n=1,shape1(2)
          result(:, copy_cursor) = array1(:,n)
          copy_cursor = copy_cursor + 1
       end do
       do n=1, shape2(2)
          result(:, copy_cursor) = array2(:,n)
          copy_cursor = copy_cursor + 1
       end do
    end if
  end function

  function horizontal_flip(array) result(result)
    integer, dimension(:,:) :: array
    integer, allocatable :: result(:,:)
    integer, dimension(2) :: shape_
    integer :: copy_cursor, col

    shape_ = shape(array)

    allocate(result, mold=array)

    copy_cursor = 1
    do col=shape_(2), 1, -1
       result(:, copy_cursor) = array(:, col)
       copy_cursor = copy_cursor + 1
    end do
  end function

  function vertical_flip(array) result(result)
    integer, dimension(:,:) :: array
    integer, allocatable :: result(:,:)
    integer, dimension(2) :: shape_
    integer :: copy_cursor, row

    shape_ = shape(array)

    allocate(result, mold=array)

    copy_cursor = 1
    do row=shape_(1), 1, -1
       result(copy_cursor, :) = array(row, :)
       copy_cursor = copy_cursor + 1
    end do
  end function



end module
