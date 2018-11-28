program testprint
  implicit none
  print*, "padded :", 5
  print '(A, I0)' , "unpadded:", 5

end program

