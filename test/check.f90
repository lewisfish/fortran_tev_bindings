Program p
    
    use tev_mod

    implicit none

    type(tevipc) :: tev
    real :: image(100, 100, 3), bonus_data(100, 100, 1), jmean(200, 200, 200)
    real :: imgin(200,200,1)

    image = 1.
    image(40:61, :, 1) = 0.
    image(:, 40:61, 2) = 0.
    image(50:71, 50:71, 3) = 0.
    jmean = 1.
    jmean(40:60, :, 50:70) = 0.

    bonus_data(:,:,1) = image(:,:,1) + image(:,:,2) + image(:,:,3)

    tev = tevipc()
    call tev%create_image("test", 100, 100, ["R", "G", "B", "S"], .true.)
    call tev%update_image("test", image(:,:,1:1), ["R"], 0, 0, .true., .true.)
    call tev%update_image("test", image(:,:,2:2), ["G"], 0, 0, .true., .true.)
    call tev%update_image("test", image(:,:,3:3), ["B"], 0, 0, .true., .true.)
    call tev%update_image("test", bonus_data, ["S"], 0, 0, .true., .true.)

    call tev%open_image("../glass.png", "", .true.)
    call tev%reload_image("../glass.png", .true.)

    call tev%create_image("jmean", 200, 200, ["R"], .true.)
    imgin = reshape(jmean(:,50:50,:), [200,200,1])
    call tev%update_image("jmean", imgin(:,:,1:1), ["R"], 0, 0, .true., .false.)

    call sleep(5)
    call tev%close_image("test")
    call tev%close_image("jmean")

end program p