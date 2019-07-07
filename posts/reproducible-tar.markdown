# Reproducible `tar`

If you want to build a tar file in a reproducible way how would you do it? 
For the sake of argument say you want to preserve

* directory structure,
* directory and file names, and
* whether the file is executable or not.

[reproducible-builds.org
suggests](https://reproducible-builds.org/docs/archives/)

    # requires GNU Tar 1.28+
    $ tar --sort=name \
          --mtime="@${SOURCE_DATE_EPOCH}" \
          --owner=0 --group=0 --numeric-owner \
          -cf product.tar build

If you have GNU Tar < 1.28 then you can replace the `--sort` flag with
`find` and `sort`.  You might also want to use `--mode="go-rwx,u-rw"` to
preserve only the executable bit of the file permissions.  Additionally, I
see no reason to allow the `mtime` to vary at all.  All in, I suggest

    find <files> -print0 \
    | sort -z \
    | tar -cf <output>.tar \
          --format=posix \
          --numeric-owner \
          --owner=0 \
          --group=0 \
          --mode="go-rwx,u-rw" \
          --mtime='1970-01-01' \
          --no-recursion \
          --null \
          --files-from -

## Pax

GNU Tar uses a GNU-specific file format.   There's a somewhat more capable
format called "pax" and it's defined in the POSIX.1-2001 specification. 
[The GNU Tar
manual](https://www.gnu.org/software/tar/manual/html_node/Formats.html#SEC134)
is somewhat worrying because it says that

> [The posix] archive format will be the default format for future versions
> of GNU tar.

If you don't want to use a file format that's losing its default status in
the future you might be tempted to switch to pax now.  Unfortunately, pax
seems to have a lot of downsides for reproducible builds.  [The Wikipedia
entry](https://en.wikipedia.org/wiki/Tar_(computing)) doesn't describe the
format and [the `pax` tool does not even support the pax
format](https://en.wikipedia.org/wiki/Pax_(Unix)#Format_support)!  The best
place to learn about the pax specification is possibly from the [Open Group
Base Specification Issue
7](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html).

The single biggest downside is that pax can contain a lot of additional
fields and it might be hard to persuade your archive creation program to
create a file in a reproducible way.  For example, if you try to create a
pax archive containing one empty file, thus

    touch example \
     && tar -cf output.tar \
            --format=posix \
            --numeric-owner \
            --owner=0 \
            --group=0 \
            --mode="go-rwx,u-rw"
            --mtime='1970-01-01' \
            example \
      && hexdump -C output.tar

then you will see that pax creates `atime` and `ctime` fields in extended
pax headers.  I cannot find any way to tell GNU Tar to turn these off.

In conclusion, reproducible builds are currently best done with GNU Tar
format.

## Addendum

*ivfsurtm* contacted me to point out that it is possible to omit the
 `atime` and `ctime` fields with the following command

```
tar --format=posix
    --pax-option=exthdr.name=%d/PaxHeaders/%f,delete=atime,delete=ctime,delete=mtime
    --mtime='1970-01-01 00:00:00Z'
    --sort=name
    --owner=0
    --group=0
    --numeric-owner
    -cvf archive.tar
    examplefile
```

The options are documented in [the Gnu tar
manual](https://www.gnu.org/software/tar/manual/html_section/tar_70.html).
