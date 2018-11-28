module mapgen

  ! type, public :: 2DArray
  !    integer:: row, col
  !    integer, dimension(:) :: array

  ! end type 2DArray


contains
  function generate_random(x,y, pulse_count, pulse_value) result(result)
    integer:: x,y, pulse_count, pulse_value
    integer, dimension(x,y) :: result

    result = zeros(x,y)

    do n=1,pulse_count
       result(randint(1,x + 1), randint(1, y+1)) = pulse_value
    end do


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

end module

