program test_mapgen
  use mapgen

  integer, dimension(10,8):: array
  type(GameMap) :: foo


  print*, "print_map function"

  foo = generate_map(6, 4, 3)

  call print_map(foo%array)


end program
