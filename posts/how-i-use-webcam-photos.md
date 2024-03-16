# How I use my webcam to take still photos

```
ffmpeg -f video4linux2 -s 640x480 -i /dev/video2 -ss 0:0:2 -frames 1 /tmp/out2.jpg
```

(Thanks to [Alexandre
Schmidt](https://askubuntu.com/questions/106770/take-a-picture-from-terminal/409725#409725))
