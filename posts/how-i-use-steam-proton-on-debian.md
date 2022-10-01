# How I use Steam Proton on Debian

## Mouse coordinate transform matrix bug

If you have used `xinput` to change your mouse's coordinate transform
matrix this may cause a problem with Steam Proton where the mouse gets
stuck in the top-left corner.  In a first person game this will
manifest as the camera turning upwards and to the left (or downwards
to the left if the mouse is inverted).  The solution is to reset your
coordinate transformation matrix to the default.

1. Run `xinput list` to see the list of devices, and note the name of
   the mouse device, for example `Logitech USB Optical Mouse`.

2. Run `xinput list-props "Logitech USB Optical Mouse"` to see the
   list of properties, and note the name of the coordinate
   tranformation matrix property, probably `Coordinate Transformation
   Matrix`.

3. Run

  ```
  xinput set-prop "Logitech USB Optical Mouse" "Coordinate Transformation Matrix" 1 0 0 0 1 0 0 0 1
  ```

  to set the coordinate transformation matrix to its default value.

## References

* <https://i.reddit.com/r/linux_gaming/comments/3nxnzg/mouse_stuck_in_topleft_corner_please_help/>

* [how-i-use-baba-is-you/](../how-i-use-baba-is-you/)
