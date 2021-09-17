# How I use cryptsetup and LUKS


* LUKS device configuration

  ```
  $ DEVICE=/dev/sdc
  $ MAPPING_NAME=sdc
  $ MOUNT_POINT=/mnt/my_mount_point
  ```

* Formatting a LUKS device

  ```
  $ sudo cryptsetup luksFormat --type luks2 $DEVICE
  ...
  $ sudo cryptsetup open $DEVICE $MAPPING_NAME
  Enter passphrase for /dev/sdc:
  $ sudo mkfs.ext4 /dev/mapper/$MAPPING_NAME
  $ sudo cryptsetup close $MAPPING_NAME
  ```

* Mounting a LUKS device

  ```
  $ sudo cryptsetup open $DEVICE $MAPPING_NAME
  Enter passphrase for /dev/sdc:
  $ sudo mount /dev/mapper/$MAPPING_NAME $MOUNT_POINT
  ```

* Unmounting an LUKS device

  ```
  $ sudo umount $MOUNT_POINT
  $ sudo cryptsetup close $MAPPING_NAME
  ```

* Making a LUKS header backup

  ```
  $ HEADER_BACKUP_FILE=header-backup-file
  $ sudo cryptsetup luksHeaderBackup --header-backup-file $HEADER_BACKUP_FILE $DEVICE
  ```

* Opening a LUKS device from a header backup

  ```
  sudo cryptsetup --header $HEADER_BACKUP_FILE luksOpen $DEVICE $MAPPING_NAME
  ```

## References

* <https://security.stackexchange.com/questions/140948/mapping-of-encrypted-to-decrypted-blocks-in-lucks>
* <https://gitlab.com/cryptsetup/cryptsetup/-/wikis/FrequentlyAskedQuestions#6-backup-and-data-recovery>
