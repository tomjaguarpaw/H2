# How I use MacOS

* By default Ctrl-Space switches keyboard layout. I disable it: <https://apple.stackexchange.com/questions/423971/disable-controlspace-keyboard-shortcut/423974#423974>

* If your tilde (~) key has become switched for plus-minus (±) then in
  the keyboard settings, choose "change keyboard type" and choose ANSI
  keyboard (not ISO).

* iTerm2 to send Alt properly: Preferences -> Profiles -> Default ->
  Keys -> Left Option Key -> Esc+

* Fix the iTerm2 flashing white line bug:
  <https://blog.sangdang.fi/fix-the-white-line-flashing-on-top-of-iterm2-in-fullscreen>

* Rebind Caps Lock to Enter:

  ```
  hidutil property --set '{"UserKeyMapping": [{"HIDKeyboardModifierMappingSrc": 0x700000039,"HIDKeyboardModifierMappingDst": 0x700000058}]}'
  ```

  (see
  <https://developer.apple.com/library/archive/technotes/tn2450/_index.html>)

* Show dock instantly

  ```
  defaults write com.apple.dock autohide-time-modifier -int 0 && killall Dock
  ```

* Disable drag lock: Accessibility -> Pointer Control -> Trackpad
  Options -> Enable Dragging -> Without Drag Lock

  But how to release drag instantly?  No one knows.

  * <https://itectec.com/askdifferent/macos-tap-and-drag-delay/>

  * <https://www.reddit.com/r/MacOS/comments/mke6p2/trackpad_drag_lock_too_long_reduce_lock/>

* Weird mouse wheel scroll acceleration.  This doesn't seem to work:

  ```
  defaults write .GlobalPreferences com.apple.scrollwheel.scaling -1
  ```

  <https://apple.stackexchange.com/a/256512/270818>

  Haven't tried this: <https://github.com/davekeck/DisableExtremeScrollAcceleration>

* Chrome undo close tab: Command-Shift-T or File -> Undo Close Tab

* iTerm2 dim inactive background windows: iTerm2 -> Preferences ->
  Appearances -> Dimming -> Dim Inactive Background Windows

* [macOS Setup after 15 Years of
  Linux](https://news.ycombinator.com/item?id=29742551)

* Possibly how to make it sleep better:

  <https://news.ycombinator.com/item?id=29743452>
