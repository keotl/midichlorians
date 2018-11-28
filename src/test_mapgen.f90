program test_mapgen
  use mapgen

  integer, dimension(3,4):: array

  array = generate_random(3,4, 4, 255)
  ! array = zeros(3,4)

  do n=1,10
     print*, n
  end do

  do i_x=1,2
     print*, array(1:2, 1)
  end do

  print*, "print_map function"

  call print_map(array)

  print*, randint(5,25)

end program
