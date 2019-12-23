program intel_hello

    implicit none

        write(*,*) "Hello from image ", this_image ( ), " out of ", num_images ( )," total images"

end program intel_hello
