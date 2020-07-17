* `MEM=$[1000 * 1000]; ulimit -d $MEM; ulimit -m $MEM; ulimit -v $MEM`

  I'm not sure which of these flags actually limits the memory to 1
  GB, but one of them does!

* `ulimit -a`
