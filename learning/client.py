with open("mypipe") as fifo:
    print("hello")
    print("age:")
    age = fifo.readline()

    print(age)
    with open("tofortran", "w") as out:
        out.write("this is a returned value: 123\n")
