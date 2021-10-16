# How I use Insurgency on Debian

## Install steam

See <https://wiki.debian.org/Steam>

## Run the game

```
cd ~/.local/share/Steam/steamapps/common/insurgency2
./insurgency.sh
```

or try

```
SteamEnv=1 ./insurgency.sh

## MESA-LOADER: failed to open iris

```
libGL error: MESA-LOADER: failed to open iris: /home/username/.local/share/Steam/steamapps/common/insurgency2/bin/libgcc_s.so.1: version `GCC_7.0.0' not found
```

Just move it out of the way

```
mv ~/.local/share/Steam/steamapps/common/insurgency2/bin/libgcc_s.so.1 ~/.local/share/Steam/steamapps/common/insurgency2/bin/libgcc_s.so.1.b
```

If you get the following error the above will probably fix it too.

```
libGL error: MESA-LOADER: failed to open iris: /usr/lib/dri/iris_dri.so: cannot open shared object file: No such file or directory (search paths /usr/lib/i386-linux-gnu/dri:\$${ORIGIN}/dri:/usr/lib/dri)
```

If it does not you could try:

```
sudo mkdir -p /usr/lib/dri
sudo ln -s /usr/lib/i386-linux-gnu/dri/iris_dri.so /usr/lib/dri/iris_dri.so
```

## SetLocale('en_US.UTF-8') failed

```
sudo dpkg-reconfigure locales
```

Choose `en_US.UTF-8` (amongst the others that you want).  Then restart
`steam` (not the game).


## Complaints about `libopenal1`

```
sudo apt-get install libopenal1:i386
```
```

## Changing the gamma

```
xgamma -gamma 1.8
```
