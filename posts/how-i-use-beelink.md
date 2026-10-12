# How I use Beelink

I successfully installed Debian 13 (Trixie) onto a Beelink SEi8. There
are a couple of important points to note:

* To access the boot menu, unsure the USB keyboard is plugged directly
  in to the Beelink, not via a hub.

* Install `firmware-intel-graphics` in order to get the full graphics
  resolution in X.

## `initramfs`

When installing a new Debian kernel through `apt`, the `initramfs` is
built, by default, with the `MODULES=dep` setting, which detects the
hardware currently plugged into the device and only adds support for
hardware it finds. This means that if you run your Beelink headless it
will not boot with support for a USB keyboard.  If you use LUKS, you
will not be able to type your password on next boot. To get around
this:

* `sudoedit /etc/initramfs-tools/conf.d/driver-policy`
* Change `MODULES=dep` to `MODULES=most`
* `sudo update-initramfs -u -k 6.12.111+deb13-amd64`
* Verify it contains USB input support:

  ```
  lsinitramfs /boot/initrd.img-6.12.111+deb13-amd64 |
    grep -E 'xhci|usbcore|usbhid|hid-generic|evdev'
  ```
