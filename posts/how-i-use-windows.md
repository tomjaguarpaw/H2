# How I use Windows

## Keyboard

### Dvorak layout

* "Ask me anything"
* Type "Control Panel"
* Load the Control Panel
* Change Input Methods

* Add a language
* English (United Kingdom)
* Options
* Add an input method
* United States-Dvorak

* Advanced Settings
* Override for default input method
* United States-Dvorak

### Disable Control-Shift changing keyboard layouts

* Settings
* Language
* Keyboard
* Input Language Hot Keys
* Change key sequence
* Not assigned

## Change Caps Lock key to deliver Enter

Run `caps-enter-local-machine.reg` (caps-enter.reg doesn't seem to
work). May have to restart.

`caps-enter-local-machine.reg`

```
[HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout]
"Scancode Map"=hex:00,00,00,00,00,00,00,00,02,00,00,00,1c,00,3a,00,00,00,00,00
```

`caps-enter.reg`:

```
REGEDIT4
[HKEY_CURRENT_USER\Keyboard Layout]
"Scancode Map"=hex:00,00,00,00,00,00,00,00,02,00,00,00,1d,00,3a,00,00,00,00,00
```

## Mouse

### Invert mouse wheel

* `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Enum\HID`

* Expand the key that matches the VID value for your mouse and the one
  under it. (You can discover the in Device Manager)

* Set Device Parameters\FlipFlopWheel to 1

* (I needed to restart.  Logging out and back in was not enough.)
