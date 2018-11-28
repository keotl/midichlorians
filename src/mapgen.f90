module mapgen
  use array_manipulation
  type, public :: GameMap
     integer:: row, col
     integer, pointer :: array(:,:)

  end type GameMap

contains

  type(GameMap) function generate_map(player_count) result(result)
    ! allowed player counts : 2, 4, 6, 9
    integer:: player_count
    logical :: horizontal_symmetry
    logical :: vertical_symmetry
    integer, pointer :: single_matrix(:,:), duplicated_matrix(:,:)

    horizontal_symmetry = player_count .ne. 9
    vertical_symmetry = player_count .ne. 9 .and. player_count .ne. 2

    allocate(single_matrix(10,8))

    single_matrix = generate_random_matrix(10,8, 14, 200, horizontal_symmetry, vertical_symmetry)

    call print_map(single_matrix)

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

