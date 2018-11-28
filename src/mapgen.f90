module mapgen
  use array_manipulation
  type, public :: GameMap
     integer, allocatable :: array(:,:)

  end type GameMap

contains

  type(GameMap) function generate_map(player_count, rows, columns) result(result)
    ! allowed player counts : 2, 4, 6, 9
    integer:: player_count
    logical :: horizontal_symmetry
    logical :: vertical_symmetry
    integer, allocatable :: single_matrix(:,:), duplicated_matrix(:,:), tiled_matrix(:,:)
    integer :: rows, columns

    horizontal_symmetry = player_count .ne. 9
    vertical_symmetry = player_count .ne. 9 .and. player_count .ne. 2


    allocate(single_matrix(rows,columns))
    single_matrix = generate_random_matrix(rows,columns, rows + 4, 200, horizontal_symmetry, vertical_symmetry)

    if (player_count == 2) then
       allocate(tiled_matrix(rows, columns * 2))
       tiled_matrix = concatenate(single_matrix, horizontal_flip(single_matrix), HORIZONTAL)
    else if (player_count == 4) then
       tiled_matrix = tile_quarters(single_matrix)
    else if (player_count == 6) then
       tiled_matrix = tile_sixths(single_matrix)
    end if

    result = GameMap(tiled_matrix)

  end function

  function tile_quarters(array) result(result)
    integer , allocatable :: array(:,:), result(:,:), half_map(:,:)
    integer, dimension(2) :: shape_
    shape_ = shape(array)

    allocate(half_map(shape_(1)*2, shape_(2)))
    half_map = concatenate(array, vertical_flip(array), VERTICAL)
    allocate(result(shape_(1)*2, shape_(2)*2))
    result = concatenate(half_map, horizontal_flip(half_map), HORIZONTAL)
  end function

  function tile_sixths(array) result(result)
    integer, allocatable :: array(:,:), result(:,:), half_map(:,:), left_intermediary(:,:)
    integer, dimension(2) :: shape_
    shape_ = shape(array)

    allocate(left_intermediary(shape_(1)*2, shape_(2)))
    left_intermediary = concatenate(array, array, VERTICAL)

    allocate(half_map(shape_(1)*3, shape_(2)))
    half_map = concatenate(left_intermediary, array, VERTICAL)

    allocate(result(shape_(1)*3, shape_(2)*2))
    result = concatenate(half_map, horizontal_flip(half_map), HORIZONTAL)
  end function

  function generate_random_matrix(x,y, pulse_count, pulse_value, horizontal_symmetry, vertical_symmetry) result(result)
    integer:: x,y, pulse_count, pulse_value
    integer, dimension(x,y) :: result
    integer, dimension(3,3) :: kernel
    logical :: horizontal_symmetry, vertical_symmetry

    result = zeros(x,y)

    do n=1,pulse_count
       result(randint(1,x), randint(1, y)) = pulse_value
    end do

    kernel = reshape((/0, 1, 0, 1, 4, 1, 0, 1, 0/), (/3,3/))

    call convolute(result, kernel, horizontal_symmetry, vertical_symmetry, 8)
    call convolute(result, kernel, horizontal_symmetry, vertical_symmetry, 8)

  end function


  integer function randint(min, max)
    ! max not included
    integer,intent(in):: min, max
    real:: rand
    call random_number(rand)
    randint = min + floor(rand * max)
  end function

   end module

