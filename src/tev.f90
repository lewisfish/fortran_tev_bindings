module tev_mod
    
    use f90sockets

    implicit none

    type :: tevipc
        character(len=:), allocatable :: hostname
        integer :: socket, inet, port
        contains
        procedure :: open_image
        procedure :: close_image
        procedure :: reload_image
        procedure :: create_image
        procedure :: update_image
    end type tevipc

    interface tevipc
        module procedure tev_init
    end interface tevipc

    contains

    type(tevipc) function tev_init(hostname, port, inet)
    ! init tevipc class with hostname, port number and inet value
    ! Opens socket with these values
    !
        implicit none

        character(len=1024), intent(IN), optional :: hostname
        integer,             intent(IN), optional :: port, inet

        character(len=1024) :: hostname_def
        integer :: port_def, inet_def

        if(present(hostname))then
            hostname_def = hostname
        else
            hostname_def = "127.0.0.1"//achar(0)
        end if

        if(present(port))then
            port_def = port
        else
            port_def = 14158
        end if

        if(present(inet))then
            inet_def = inet
        else
            inet_def = 1
        end if

        tev_init%hostname = hostname_def
        tev_init%port = port_def
        tev_init%inet = inet_def

        call open_socket(tev_init%socket, tev_init%inet, tev_init%port, tev_init%hostname)

    end function tev_init

    subroutine open_image(this, path, channel_selector, grab_focus)
    ! open image at path with channels: channel_selector

        implicit none
    
        class(tevipc) :: this    
        character(len=*), intent(IN) :: path
        character(len=*), intent(IN), optional :: channel_selector
        logical,          intent(IN), optional :: grab_focus

        character(len=:), allocatable :: buf
        character(len=:), allocatable :: cs
        logical :: focus
        integer :: i, ptr

        if(present(grab_focus))then
            focus = grab_focus
        else
            focus = .true.
        end if

        if(present(channel_selector))then
            cs = channel_selector
        else
            cs = ""
        end if

        buf = repeat(achar(0),len(path) + len(cs) + 9)
        !buffer size
        buf(1:4) = transfer(len(buf), buf(1:4))
        !operation open_image
        buf(5:5) = achar(7)
        if(focus)then
            buf(6:6) = achar(1)
        else
            buf(6:6) = achar(0)
        end if
        
        do i = 7, len(path) + 6
            buf(i:i) = path(i-6:i-6)
        end do

        ptr = i
        do i = 1, len(cs)
            buf(ptr:ptr) = cs(i:i)
        end do
        buf(ptr:ptr) = achar(0)

        call writebuffer(this%socket, buf, len(buf)) ! write data
    end subroutine open_image


    subroutine close_image(this, name)
    ! close image name
        
        implicit none
    
        class(tevipc) :: this    
        character(len=*), intent(IN) :: name
    
        character(len=:), allocatable :: buf
        integer :: i

        buf = repeat(achar(0), 4 + len(name) + 2)

        buf(1:4) = transfer(len(buf), buf(1:4))!size of packet
        buf(5:5) = achar(2)!close image
        !name of image
        do i = 6, len(name) + 5
            buf(i:i) = name(i-5:i-5)
        end do

        call writebuffer(this%socket, buf, len(buf))

    end subroutine close_image

    subroutine create_image(this, name, width, height, channel_names, grab_focus)
    ! create image called name, with width and height (width, height) and the channel names channel_names
    !
        implicit none

        class(tevipc) :: this
        character(len=*), intent(IN) :: name
        integer,          intent(IN) :: width, height
        character(len=1), intent(IN), optional :: channel_names(:)
        logical,          intent(IN), optional :: grab_focus

        character(len=:), allocatable :: buf
        integer :: i, ptr
        character(len=1), allocatable :: chan_names(:)
        logical :: g_focus

        if(present(channel_names))then
            allocate(chan_names(len(channel_names)))
            chan_names = channel_names
        else
            allocate(chan_names(4))
            chan_names = ["R", "G", "B", "A"]
        end if

        if(present(grab_focus))then
            g_focus = grab_focus
        else
            g_focus = .true.
        end if

        buf = repeat(" ", 4 + 2 + len(name) + 1 + 8 + 4 + 2*size(channel_names))

        buf(1:4) = transfer(len(buf), buf(1:4))!size of packet
        buf(5:5) = achar(4)!create image
        if(g_focus)then
            buf(6:6) = achar(1)!grab_focus
        else
            buf(6:6) = achar(0)
        end if

        !name of image
        do i = 7, len(name) + 6
            buf(i:i) = name(i-6:i-6)
        end do

        ptr = i
        buf(ptr:ptr) = achar(0)
        ptr = ptr + 1

        !width
        buf(ptr:ptr+4) = transfer(width, buf(ptr:ptr+4))
        ptr = ptr + 4
        !height
        buf(ptr:ptr+4) = transfer(height, buf(ptr:ptr+4))
        ptr = ptr + 4

        !channels
        buf(ptr:ptr+4) = transfer(size(channel_names), buf(ptr:ptr+4))
        ptr = ptr + 4
        do i = 1, size(channel_names)
            buf(ptr:ptr) = channel_names(i)
            ptr = ptr + 1
            buf(ptr:ptr) = achar(0)
            ptr = ptr + 1
        end do

        call writebuffer(this%socket, buf, len(buf)) ! writing data

    end subroutine create_image


    subroutine reload_image(this, name, grab_focus)
    ! reload image name
        
        use f90sockets

        implicit none

        class(tevipc) :: this    
        character(len=*),  intent(IN) :: name
        logical, optional, intent(IN) :: grab_focus
    
        integer :: i
        logical :: gf
        character(len=:), allocatable :: buf
    
        if(present(grab_focus))then
            gf = grab_focus
        else
            gf = .true.
        end if

        buf = repeat(" ", 4 + 3 + len(name))

        buf(1:4) = transfer(len(buf), buf(1:4))!size of packet
        buf(5:5) = achar(1)!reload image
        if(gf)then
            buf(6:6) = achar(1)!grab_focus
        else
            buf(6:6) = achar(0)!grab_focus
        end if        

        !name of image
        do i = 7, len(name) + 6
            buf(i:i) = name(i-6:i-6)
        end do

        buf(i:i) = achar(0)

        call writebuffer(this%socket, buf, len(buf))

    end subroutine reload_image

    subroutine update_image(this, name, image, channel_names, x, y, grab_focus, perfom_tiling)
    ! update image call name with image data from coordinate (x,y)
    ! can be tiled if perform_tiling is true
    !
        use iso_fortran_env, only : int64

        implicit none
    
        class(tevipc) :: this
        character(len=*),  intent(IN) :: name
        real,              intent(IN) :: image(:, :, :)
        character(len=1),  intent(IN) :: channel_names(:)
        integer, optional, intent(IN) :: x, y
        logical, optional, intent(IN) :: grab_focus, perfom_tiling
    
        real,             allocatable :: tile(:, :, :)
        integer,          allocatable :: channel_offsets(:), channel_strides(:)
        character(len=:), allocatable :: buf
        integer                       :: tile_size(2), i, j, n_channels, k, ptr, l, m, xx, yy
        logical :: gf, pt

        if(present(x))then
            xx = x
        else
            xx = 0
        end if

        if(present(x))then
            yy = y
        else
            yy = 0
        end if

        if(present(perfom_tiling))then
            pt = perfom_tiling
        else
            pt = .true.
        end if

        if(pt)then
            tile_size = [128, 128]
        else
            tile_size = [size(image,1), size(image,2)]
        end if
    
        n_channels = size(image,3)
        allocate(channel_offsets(n_channels), channel_strides(n_channels))

        channel_offsets = [(i,i=0,n_channels-1)]
        channel_strides = [(n_channels,i=0,n_channels-1)]

        allocate(tile(tile_size(1), tile_size(2), n_channels))

        do i = 0, size(image, 1)-1, tile_size(1)
            do j = 0, size(image, 2)-1, tile_size(2)
                tile = image(i+1:min(i+1+tile_size(1), size(image, 1)), j+1:min(j+1+tile_size(2), size(image, 2)), :)
                buf = repeat(" ",sizeof(tile) + len(name) + 2*size(channel_names) &
                             + 8*size(channel_strides) + 8*size(channel_offsets) + 42)


                buf(1:4) = transfer(len(buf), buf(1:4))!size of packet
                buf(5:5) = achar(6)!update image

                if(present(grab_focus))then
                    gf = grab_focus
                else
                    gf = grab_focus
                end if
                
                if(gf)then
                    buf(6:6) = achar(1)!grab_focus
                else
                    buf(6:6) = achar(0)!grab_focus
                end if

                ptr = 7
                !name of image
                do k = 1, len(name)
                    buf(ptr:ptr) = name(k:k)
                    ptr = ptr + 1
                end do

                buf(ptr:ptr) = achar(0)
                ptr = ptr + 1
                !number of channels
                buf(ptr:ptr+4) = transfer(n_channels, buf(ptr:ptr+4))
                ptr = ptr + 4

                !channel names
                do k = 1, size(channel_names)
                    buf(ptr:ptr) = channel_names(k)
                    ptr = ptr + 1
                    buf(ptr:ptr) = achar(0)
                    ptr = ptr + 1
                end do

                !update position
                buf(ptr:ptr+4) = transfer(xx+j, buf(ptr:ptr+4))
                ptr = ptr + 4
                buf(ptr:ptr+4) = transfer(yy+i, buf(ptr:ptr+4))
                ptr = ptr + 4

                !update image size
                buf(ptr:ptr+4) = transfer(size(tile, 2), buf(ptr:ptr+4))
                ptr = ptr + 4
                buf(ptr:ptr+4) = transfer(size(tile, 1), buf(ptr:ptr+4))
                ptr = ptr + 4

                !channel offsets
                do k = 1, size(channel_offsets)
                    buf(ptr:ptr+8) = transfer(int(channel_offsets(k),kind=int64), buf(ptr:ptr+8))!size of packet
                    ptr = ptr + 8
                end do

                !channel strides
                do k = 1, size(channel_strides)
                    buf(ptr:ptr+8) = transfer(int(channel_strides(k),kind=int64), buf(ptr:ptr+8))!size of packet
                    ptr = ptr + 8
                end do

                !image data
                do k = 1, size(tile, 1)
                    do l = 1, size(tile, 2)
                        do m = 1, size(tile, 3)
                        buf(ptr:ptr+4) = transfer(tile(k,l,m), buf(ptr:ptr+4))
                        ptr = ptr + 4
                        end do
                    end do
                end do

                call writebuffer(this%socket, buf, len(buf))
                deallocate(buf)
            end do
        end do

    end subroutine update_image
end module tev_mod
