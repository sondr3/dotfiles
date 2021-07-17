# Arch installation and configuration

## Configure bootloader

```
$ cat /boot/loader/entries/arch.conf
title	Arch Linux
linux	/vmlinuz-linux-zen
initrd	/intel-ucode.img
initrd	/initramfs-linux-zen.img
options	root=LABEL=ROOT rw mitigations=off
options resume=LABEL=SWAP
```

### Set `resume` in initramfs

1. `HOOKS=(base udev autodetect modconf block filesystems keyboard resume fsck)`
2. `sudo mkinitcpio`

## Install `pipewire`

1. Install `pipewire xdg-desktop-portal-wlr pipewire-pulse`
2. `systemctl --user enable --now pipewire.service`
3. `systemctl --user enable --now pipewire-pulse.service`

## Pacman configuration

1. Run `sudo reflector --latest 10 --sort rate --protocol https --country Norway,Sweden --save /etc/pacman.d/mirrorlist` to set latest mirrors
2. Enable the following options in `/etc/pacman.conf`:
   ```text
   Color 
   CheckSpace
   ParallelDownloads = 5 
   ILoveCandy
   ```

## Assorted

- Enable `fstrim`: `systemctl enable fstrim.timer`

## Laptops

### Configure Bluetooth

1. `paru -S bluez bluez-utils`
2. `systemctl enable --now bluetooth.service`

