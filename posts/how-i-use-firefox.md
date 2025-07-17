# How I use Firefox

* Create a profile using the profile manager

  ```
  firefox -no-remote -profileManager
  ```

  I have different profiles for different usages of Firefox, to keep
  them separate. Hopefully that increases my privacy a bit. For
  example I have separate profiles for general browsing, Twitter,
  WhatsApp, Facebook, Google Calendar and my bank website.

* Launch Firefox with a profile with

  ```
  firefox -no-remote -P "name_of_the_profile"
  ```

* Set prefers colour scheme to dark

  > You can update the style used by Firefox by going to `about:config`
  > and adding a new property `ui.systemUsesDarkTheme` of integer type
  > with value `1`.

 From
 [StackOverflow](https://stackoverflow.com/questions/56401662/firefox-how-to-test-prefers-color-scheme/56757527#56757527)

* Make everything dark with <https://github.com/SrKomodo/shadowfox-updater>


* Reduce Firefox's memory consumption

  <https://support.mozilla.org/en-US/kb/firefox-uses-too-much-memory-or-cpu-resources?redirectslug=firefox-uses-too-much-memory-ram&redirectlocale=en-US>

  Performance

  uncheck Use recommended performance settingsLearn more

  Content process limit -> 1

* Disable "Ctrl-Tab cycles through tabs in recently used order"

* [`browser.search.context.loadInBackground =
  true`](https://support.mozilla.org/en-US/questions/929983#answer-34412)

## Config

* Set `full-screen-api.ignore-widgets` to `false` (which is probably
  the default) otherwise `xmonad` fullscreen doesn't cover xmobar.

* Disable JIT compilation

  ```
  javascript.options.baselinejit
  javascript.options.wasm_baselinejit
  javascript.options.wasm_optimizingjit
  javascript.options.ion
  ```

## Disable sponsored sites

`about.config`:

```
browser.newtabpage.activity-stream.showSponsored false
browser.newtabpage.activity-stream.showSponsoredTopSites false
browser.newtabpage.activity-stream.system.showSponsored false
```

## Don't show sidebar when using tree-style tabs

`about.config`:

```
sidebar.revamp false
```

## Issues

There is a very annoying bug whereby the volume of a video, set with
pulseaudio, resets on interactions such as forward, rewind, and
various others.  See
<https://bugzilla.mozilla.org/show_bug.cgi?id=1422637>.
