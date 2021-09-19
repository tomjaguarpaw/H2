# How I use HP Deskjet 1050A on Debian GNU/Linux

## Printing

I use CUPS to manager the printer.  Install `cups` to get it.  The
default driver for this printer that CUPS chooses does not work!  (`HP
DesignJet 1050c pcl, 1.0 (color)`).  Instead I installed `hplip` and
`printer-driver-hpcups` (perhaps only one of these was required) and
chose the driver `HP Deskjet 1050 j410 Series, hpcups 3.21.2 (color)`.

I tend to prefer to use "fast draft" quality, which can be set in the
CUPS administration options page.

## Scanning

Seems to require the `hplip` and `libsane-hpaio` packages.  I use
`simple-scan` for scanning.  I had to `chown` the device file
`/dev/bus/usb/002/005`.

If `xscanimage` or `sane-find-scanner` report that no scanner was
identified then you probably need one of the above pacakges.
