program test_array_manipulation
  use array_manipulation
  integer, dimension(3,3)::array1, array2
  integer, allocatable :: array3(:,:)

  do i=1,3
     do j=1,3
        array1(i,j) = 1
        array2(i,j) = 2
     end do
  end do

  call print_map(array1)
  call print_map(array2)

  print*, "Horizontal Concatenate"
  array3 = concatenate(array1, array2, HORIZONTAL)
  call print_map(array3)
  deallocate(array3)

  print*, "Vertical Concatenate"
  array3 = concatenate(array1, array2, VERTICAL)
  call print_map(array3)
  deallocate(array3)

  print*, "Horizontal Flip"
  allocate(array3(2,2))
  array3 = reshape((/1,2,3,4/), (/2,2/))
  call print_map(array3)

  array3 = horizontal_flip(array3)
  call print_map(array3)
  deallocate(array3)

  print*, "Vertical Flip"
  allocate(array3(2,2))
  array3 = reshape((/1,2,3,4/), (/2,2/))
  call print_map(array3)

  array3 = vertical_flip(array3)
  call print_map(array3)

end program
