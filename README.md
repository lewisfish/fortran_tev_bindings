# Fortran TEV bindings

Fortran bindings for [TEV](https://github.com/Tom94/tev/)

Implments all operations:

- open_image
- close_image
- reload_image
- update_image
  
Uses f90sockets library from [https://github.com/b-fg/f2py-sockets](https://github.com/b-fg/f2py-sockets) 

## Usage

```
    type(tevipc) :: tev
    real :: image(100, 100, 3), bonus_data(100, 100, 1)

    image = 1.
    image(40:61, :, 1) = 0.
    image(:, 40:61, 2) = 0.
    image(50:71, 50:71, 3) = 0.

    bonus_data(:,:,1) = image(:,:,1) + image(:,:,2) + image(:,:,3)

    tev = tevipc()
    call tev%create_image("test", 100, 100, ["R", "G", "B", "S"], .true.)
    call tev%update_image("test", image(:,:,1:1), ["R"], 0, 0, .true., .true.)
    call tev%update_image("test", image(:,:,2:2), ["G"], 0, 0, .true., .true.)
    call tev%update_image("test", image(:,:,3:3), ["B"], 0, 0, .true., .true.)
    call tev%update_image("test", bonus_data, ["S"], 0, 0, .true., .true.)

    call tev%open_image("../glass.png", "", .true.)
    call tev%reload_image("../glass.png", .true.)
    
    call tev%close_image("test")
```
