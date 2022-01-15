# How I play video on Linux

-- specifically, Debian

## How to back up DVDs to a hard disk

```
% sudo apt-get install vobcopy libdvd-pkg
```

You may need to subsequently

```
sudo dpkg-reconfigure libdvd-pkg
```

Then the DVD device must be mounted, for example

```
sudo mount /dev/sr0 /mnt/<target>
```

even though you don't actually need to mention the mount directory
when you issue the backup command.  To back up to the current
directory issue:

```
vobcopy -m
```

## How to get decent audio balance from surround sound DVDs

If you try to play a DVD with a 6-channel ("5.1") surround sound audio
track from a stereo speaker setup then the audio balance will likely
be terrible.  Symptoms include too quiet dialogue and speech and too
loud music, sound effects and background noise.

I found it very hard to find a decent resolution to this problem, but
ultimately the following works well for me.  It may be possible to
make it work even better by tweaking the parameters.


```
% mpv --deinterlace=yes \
      --alang=en \
      '-af=pan="stereo|FL=0.707*FC+0.3*FL+0.1*SL+0.1*LFE|FR=0.707*FC+0.3*FR+0.1*SR+0.1*LFE"' \
      --dvd-device=VIDEO_TS/ \
      dvd://<number>
```

### Notes

* To play the first track on the DVD choose `<number>` to be `0`.  To
  play subsequent tracks increment `<number>` accordingly.

* `VIDEO_TS` is the top-level directory created by `vobcopy`.

* For old recordings the dialogue may only play from the front centre
  (`FC`) channel.  In that case you might want to omit `FL`, `FR`,
  `SL` and `SR` completely in the formula.

* It seems important to play DVD files through `--dvd-device
  ... dvd://<number>` so the correct audio track for the correct
  language can be chosen by name (here `en`).  The mapping between
  language names and audio track IDs seems to be stored in the `.IFO`
  file so if you play the `.VOB` files directly the mapping will not
  be used and you might find it hard to select the correct language.

* In theory `--audio-channels=stereo`, with or without
  `--ad-lavc-downmix=yes`, should perform the down mixing, but it
  didn't seem to make any difference for me.

## References

* <https://github.com/mpv-player/mpv/issues/1118>

* <https://github.com/mpv-player/mpv/issues/3979>

* <https://github.com/mpv-player/mpv/issues/6343>

* <https://github.com/mpv-player/mpv/issues/6563>

* <https://superuser.com/questions/852400/properly-downmix-5-1-to-stereo-using-ffmpeg/1410620#1410620>
