program communicationHost
  character(len=800)::resultvalue
  ! call execute_command_line("python3 client.py", wait=.false.)
  open(11,file="mypipe")
  write(11, *) 53

  open(12, file="tofortran")

  read(12, '(A)') resultvalue
  print*,resultvalue

end program communicationHost
