# How I use Debian GNU/Linux on Dell XPS 13 7390

## Background

I can happily report that this machine works well with Debian
11/Bullseye!

Reportedly, other options for a machine that is highly compatible with
Linux include Lenovo, [Laptop with
Linux](https://laptopwithlinux.com/),
[Slimbook](https://slimbook.es/en/), [Tuxedo
Computers](https://www.tuxedocomputers.com/index.php),
[System76](https://system76.com/), [Purism](https://shop.puri.sm/),
[Pine](https://pine64.com/) and [Framework](https://frame.work/).

### Installation

Installing Debian on this machine poses a few challenges.

* The only ports for external connectivity are USB-C ports (besides an
audio jack).  Your installation media (generally a USB drive) will
have to connect via this interface.

* The video card [is not compatible with Debian 10/Buster's default
  kernel](https://wiki.debian.org/InstallingDebianOn/Dell/Dell%20XPS%2013%207390).
  Therefore I suggest you use Debian 11/Bullseye instead.

* The WiFi card requires the non-free `firmware-iwlwifi` package.
  If you want to use the WiFi card during installation you will have
  to make this firmware available somehow.

* The SSD controller must be put into AHCI mode otherwise Debian will
  not be able to see it.

### Use

Subsequent use of the device has a few challenges of its own too.

* The USB-C ports are the only connectivity for external monitors and
  other peripherals.

* Suspend to RAM may require BIOS configuration.

## Installation process

### Installation media

I used a USB-A drive with [a standard Apple USB-C to USB-A
adapter](https://www.apple.com/uk/shop/product/MJ1M2ZM/A/usb-c-to-usb-adapter). Alternatively
you can use a USB-C drive.  To make `firmware-iwlwifi` available
during installation I used a non-free installation image.  I chose the
[XFCE Bullseye release candidate non-free live
CD](https://cdimage.debian.org/cdimage/unofficial/non-free/cd-including-firmware/bullseye_di_rc3-live+nonfree/amd64/iso-hybrid/)
but the significantly smaller [non-free
netinst](https://cdimage.debian.org/cdimage/unofficial/non-free/cd-including-firmware/bullseye_di_rc3+nonfree/amd64/iso-cd/)
would probably work fine too.

### Booting

Start the machine with the USB drive attached and press F12 to boot
into the BIOS menu.  Make sure that the SSD controller is set to AHCI
mode.  Then choose to boot from your USB drive.  (To do
so you may then have to exit the BIOS menu and enter it again by
pressing F12 again.)

### Installing

Installation should proceed as a standard Debian installation.

## Use

### Compatibility

I haven't found any features that don't work.

### External monitor

External monitor connectivity works fine from two of the three USB-C
ports.  I guess the other is not designed to output video.  (I haven't
tried both ports at the same time but I don't anticipate any
problems.)

I use a [StarTech USB-C to Display Port
adaptor](https://www.startech.com/en-gb/audio-video-products/cdp2dp)
with a standard Display Port to Display Port cable.

### X11

X11 works fine.  The only problem that I have noticed is that
switching to the virtual terminals (Ctrl-Alt-F1 etc.) is flaky and
running multiple X servers at the same time is flaky.  Sometimes it
works, sometimes it doesn't.  Sometimes it seems to hang but can be
recovered by closing the lid (i.e. putting the machine to sleep) and
then opening it again.  Sometimes it hangs and closing and opening the
lid doesn't work; in those cases I have to reboot.

#### X11 external screen disconnection bug

If I am in dual head mode (i.e. X is using the internal screen and an
external screen) and I unplug the external screen then the driver
seems not to notify X.  X still believes two displays are connected.
Subsequently it becomes impossible to switch to internal-only mode
until I plug the external screen back in.

### Suspend

Only `s2idle`/`freeze` (which are both names for S0/suspend to idle)
and `deep` (S3/suspend to RAM) seem to be supported.  (It's *possible*
that `disk` (S4/suspend to disk/"hibernate") doesn't appear here only
because I didn't make a big enough swap partition, but I am not sure.)

```
$ cat /sys/power/state
freeze mem
$ cat /sys/power/mem_sleep
s2idle [deep]
```

#### Suspend to RAM (S3, `deep` `mem`)

This is probably the variety of suspend that you actually want to
use because it has extremely low power draw.

I disabled TPM, SGX, SMM, and Absolute in the BIOS (as described
in a helpful [Reddit
post](https://www.reddit.com/r/Dell/comments/hla8yk/how_to_enable_s3_deep_sleep_on_xps_17_9700_in/)).
I don't know whether doing so was necessary.  Enter S3 sleep with

```
sudo sh -c 'echo deep > /sys/power/mem_sleep && echo mem  > /sys/power/state'
```

##### Waking the machine

You *must not* try to wake the machine by pressing the power button;
that will put it into shutdown.  Instead close and reopen the lid.

##### S3 sleep touchpad CPU bug

[There is a bug](https://bugzilla.redhat.com/show_bug.cgi?id=1847627)
that causes high CPU usage when returning from S3 sleep until the
touchpad is touched.  I had noticed that my fan kicked in around an
hour after waking but I didn't understand why until I noticed my load
average was persistently above 1.  I then spotted a mysterious process
called `irq/51-CUST0001` in `top`. Searching for references to it led
me to the above-mentioned bug report.

I suppose the workaround is to just touch the touchpad!

#### Suspend to idle (S0, `freeze`)

This feature works but I doubt it reduces power draw very much.

### Trackpad

Trackpad behaviour can be configured with `xinput`.  Run `xinput
--list` and look for the entry like `CUST0001:00 06CB:76B1 Touchpad`.
Use that string in the commands below.

#### Tap to click

To configure a tap on the trackpad to be registered as a left click
run

```
xinput --set-prop "CUST0001:00 06CB:76B1 Touchpad" "libinput Tapping Enabled" 1
```

#### Natural scrolling

For "Natural", i.e. reversed, scrolling run

```
xinput --set-prop "CUST0001:00 06CB:76B1 Touchpad" "libinput Natural Scrolling Enabled" 1
```

### Bluetooth

I haven't tested Bluetooth thoroughly but it seems to work.  Some useful notes follow.

* <https://wiki.debian.org/BluetoothUser#Pairing_using_CLI>
   * use `bluetoothctl`
   * `apt-get install pulseaudio-module-bluetooth`
   * `pactl load-module module-bluetooth-discover`
   * for mic and headphones at same time
       * pactl list cards short | grep bluez
       * pactl set-card-profile bluez_card.F0_1D_BC_FA_98_CF headset_head_unit
       * (See <https://www.igelcommunity.com/post/automatically-connect-bluetooth-headsets-and-enable-hsp-mode-for-bidirectional-audio-on-igel-os>)
   * does work with the microphone of Surface headphones but
     quality seems not great

### Auto-dimming screen on battery power

By default, when running on batter power the screen automatically dims
and brightens depending on what is on the screen.  It seems somewhat
random, but it depends in a strange way on the brightness of what is
already on the screen.  I worked out how to turn it off:

* In the BIOS (F2 at startup) go to Video then turn EcoPower off.

Then the brightness must be controlled manually, for example

```
echo 40000 > /sys/class/backlight/intel_backlight/brightness
```

## References

* The [Dell XPS 13 7390 installation reference on the Debian
wiki](https://wiki.debian.org/InstallingDebianOn/Dell/Dell%20XPS%2013%207390)

* [Kernel documentation about power
states](https://www.kernel.org/doc/Documentation/power/states.txt)

## Credits

* [hg8 on
askubutu](https://askubuntu.com/questions/696413/ubuntu-installer-cant-find-any-disk-on-dell-xps-13-9350/696414#696414)
for the AHCI mode solution
