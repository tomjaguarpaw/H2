# How I use `tunefs`

To free up reserved blocks

```
sudo tune2fs -r 1000000 /dev/mapper/<disk-device>
```

--
<https://chiaforum.com/t/ext4-users-remember-to-reduce-reserved-blocks/1972>
