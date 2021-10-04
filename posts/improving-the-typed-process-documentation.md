# Improving the `typed-process` documentation

## `process`

Haskell has a package called
[`process`](https://hackage.haskell.org/package/process) for launching
and managing processes using the system's underlying UNIX or Win32
API.  Whenever I've used it I have found it to be a solid,
well-implemented library.  On the other hand I always struggled to
piece together components to do anything more complicated than the
built-in functions like
[`callProcess`](https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html#v:callProcess).
I haven't found the library particularly *composable*.

The library does allow one to separate the configuration of processes
from launching them.  Configuration consists in defining a value of
type
[`CreateProcess`](https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html#t:CreateProcess)
which contains parameters like the executable file path, the
arguments, and determines what the standard input, output and error
streams should be.  Once one has defined a `CreateProcess` one can
launch a process by using functions like
[`readCreateProcess`](https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html#v:readCreateProcess)
which takes the process's `stdin` as a `String` argument and returns
its `stdout` as a `String` (overriding any such settings you gave when
you made the `CreateProcess`).

But there are also functions that configure and launch in one go, such
as
[`callProcess`](https://hackage.haskell.org/package/process-1.6.13.2/docs/src/System.Process.html#callProcess).
`callProcess` takes the executable path and list of arguments just
launches that process, no separate configuration step in sight.  For
some reason I find the mixing of two-stage functions with one-stage
functions in the same API really hard to get my head around.

Beyond the mixing that confused me there is also the daunting monster
`createProcess`, the most general way to launch a process.

```haskell
createProcess :: CreateProcess
              -> IO (Maybe Handle, Maybe Handle, Maybe Handle,
                     ProcessHandle)
```

I find it daunting mostly because of the three `Maybe Handle`s (which
correspond to the process's input and output streams).  Firstly,
`Handle` is a rather low-level concept.  Secondly, each of those
`Maybe`s is `Just` if and only if the corresponding `std_in`,
`std_out` or `std_err` field on the `CreateProcess` is set to
[`CreatePipe`](https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html#v:CreatePipe).
This invariant is documented on
[`createProcess`](https://hackage.haskell.org/package/process-1.6.13.2/docs/System-Process.html#v:createProcess)
but it should be guaranteed by the type!

Whilst trying to get my head around how `process` works I made a
couple of small
contributions[[1](https://github.com/haskell/process/pull/194/files)][[2](https://github.com/haskell/process/pull/193/files)]
to the documentation.

I recalled hearing that the
[`typed-process`](https://hackage.haskell.org/package/typed-process)
package was a process API with better type-level guarantees so I
decided to check it out.

## `typed-process`

My first foray into `typed-process` ended almost as soon as it
started.  I found the documentation more daunting than
`createProcess`!  You can look at [the Haddock page as it was
then](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html).
All the functions have documentation but they're in one long, almost
uniform, list.  Without additional structure I got lost.  Furthermore
each type signature was on a long line which only wrapped at the end
of the line, for example

```haskell
withProcessTerm_ :: MonadUnliftIO m => ProcessConfig stdin stdout stderr -> (Process stdin stdout stderr -> m a) -> m a
```

I am a strong believer that types are great documentation but the
types need space to speak for themselves and here they were being
suffocated.  I couldn't work out how to navigate.

To the credit of the package it says at the top

> Please see the README.md file for examples of using this API.

and the README.md is a good introduction.  But there was no link to
take the reader there in one click.  Little sources of friction like
this are very discouraging to me when I am a new user.  Small
breadcrumbs to guide the way are very welcome!

## Back to `process`

Discouraged by `typed-process` I returned to `process` to try to work
out how to impose the type-level guarantees I wanted.  Whilst
undertaking this work I significantly improved my understanding about
the internals of `process`.  Finally I worked out how to apply the
type-level guarantees I wanted by using a type-level `Maybe` but the
change was such a large departure from the current API that I guessed
it would never be accepted.

## Back to `typed-process`

I decided to look more closely at `typed-process`. I discovered that
it already had the type-level guarantees I wanted!  Better than that,
it supports accessing the three standard streams in a well-typed way,
at higher-level types than `Handle`, for example `ByteString`.  It
also firmly decides on a clear separation between configuring a
process (using
[`ProcessConfig`](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#t:ProcessConfig))
and launching it (with functions like
[`runProcess`](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#v:runProcess)).
The components that `typed-process` provides compose together very
neatly.  It makes life much more convenient than `process`.

I was so impressed by the library that I decided to help improve the
documentation in the hope of reducing the chance that someone will be
discouraged from it like I initially was.

## Improving the documentation

Below is the list of all improvements I made.  You can see the
documentation [before my
changes](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html)
and
[afterwards](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html).
Some of the changes (like "added an example") are general to
documentation of any software package and some (like "explained
`mkStreamSpec`) are specific to `typed-process`.

* Added a getting started example at the [top of the
  page](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html)

  The example (copied below) is very short but provides a simple way
  to get started.

  ```
  {-# LANGUAGE OverloadedStrings #-}

  runProcess "ls -l /home" >>= print
  ```

* Linked to the [README](https://github.com/fpco/typed-process#readme)

  The README, which contains a simple tutorial, [was already mentioned
  at the top of the
  page](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html)
  but [now it is a
  link](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html).
  It might sound silly or minor but links like this *really* reduce
  cognitive load for someone coming to the library for the first time.

* Forced type signatures to wrap

  This is perhaps the single biggest readability improvement that I
  made.  Instead of [wrapping at the end of the
  line](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#v:withProcessTerm_)

  ```haskell
  withProcessTerm_ :: MonadUnliftIO m => ProcessConfig stdin stdout stderr -> (Process stdin stdout stderr -> m a) -> m a
  ```

  [the signatures now wrap at each
  argument](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#v:withProcessTerm_)

  ```haskell
  withProcessTerm_
    :: MonadUnliftIO m
    => ProcessConfig stdin stdout stderr
    -> (Process stdin stdout stderr -> m a)
    -> m a
  ```

  This format is achieved by adding dummy Haddock comments (`-- ^`)
  between each argument (see [the commit in
  question](https://github.com/fpco/typed-process/commit/9eceb7bcc7c690c1933c9216f33c74913e613d1e#diff-eb95c5fd1cdf7d1916480924a11a3cfe7f450570d2fc1dacd3d4b5d183de7031)).

* Moved the commonly-used functions higher than the less-used
  functions

  For example `mkStreamSpec`, with its terrifying type signature,
  [used to be documented at the top of the "Stream specs"
  section](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#g:5)
  even though most users will only ever use the built-in
  `StreamSpec`s.  Now it appears [in a sub-section of its
  own](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#g:7)
  beneath the built-in `StreamSpec`s.

  Similarly, `startProcess` and `stopProcess` [used to be documented
  at the top of the "Launch a process"
  section](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#g:6)
  even though you're not suppose to use them.  You're supposed to use
  one of the `withProcess...` functions instead.  Even *those*
  functions should be rarely used yet they took up positions 3-8.  I
  [reordered
  them](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#v:withProcessWait)
  so that less-used functions appear lower down.  The functions that
  you are most likely to want, `runProcess` and `readProcess`, [now
  appear at the top of the
  section](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#g:8).

  Deprecated functions are not supposed to be used yet [their
  documentation was wasting space slap-bang in the
  middle](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#v:withProcess)
  of functions you *are* supposed to use.  I moved them [to their own
  section right at the
  bottom](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#g:15)
  of the module, out of sight, out of mind.  Additionally, other parts
  of the documentation referred to use of the deprecated functions.  I
  changed them to refer to the replacements instead.

* Separated exception-throwing functions into their own section

  Every process-launching function has two variants: a
  non-exception-throwing variant and an exception-throwing variant.
  The name of the latter differs from the name of the former by ending
  in an underbar.  The two variants used to appear next to each other
  (see, for example, [`readProcess` and
  `readProcess_`](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#v:readProcess)
  ).  Since there is such a tight correspondence between the names and
  functionality of the non-exception-throwing and exception-throwing
  variants I decided to separate the exception-throwing functions into
  [their own
  sub-section](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#g:9).
  Subsequently it requires less scrolling to see all the process-launching
  functions of the sort you want.

* Clarified the `mkStreamSpec` invariant

  `mkStreamSpec` is the only place where a user has to be aware of the
  `Just`/`CreatePipe` invariant mentioned above in the discussion of
  `createProcess`.  The old documentation [used to mysteriously
  say](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#v:mkStreamSpec)
  "This will be determined by the StdStream argument".  [I changed it
  to mention the precise
  invariant](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#v:mkStreamSpec)
  and explained that the `createProcess` documentation provides more
  details.

* Expanded the `StreamSpec` documentation

  It [used to
  say](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#t:StreamSpec)

  > A specification for how to create one of the three standard child
  > streams.

  but I found that rather mysterious.  What's a "standard child
  stream"?  I [added an
  explanation](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#t:StreamSpec)
  that it is one of `stdin`, `stdout` and `stderr`, along with other
  additional documentation.

* Added internal links

  The `StreamSpec` documentation [used
  to](https://hackage.haskell.org/package/typed-process-0.2.6.0/docs/System-Process-Typed.html#t:StreamSpec)
  refer to the "Stream specs" section by saying "See examples below".
  [I changed it to link
  directly](https://hackage.haskell.org/package/typed-process-0.2.6.3/docs/System-Process-Typed.html#t:StreamSpec)
  to the "Stream specs" section.  I added some other links too.  The
  links make it much easier to navigate around the page.

  Arbitrary Haddock internal links are possible using [a technique
  explained on
  StackOverflow](https://stackoverflow.com/questions/51572074/how-to-link-to-a-named-chunk-of-documentation-in-haddock/69424633#69424633)
  by [sjakobi](https://stackoverflow.com/users/1013393/sjakobi).


* Updated the links to the package and documentation home page.

  It migrated to GitHub since the library was written.

## Conclusion

`typed-process` was always a great library but I couldn't tell at
first because I couldn't make my way into the documentation.
Hopefully my improvements make the library more accessible to
newcomers.  `typed-process` is designed such that the types are an
integral part of the documentation.  My aim was to arrange the
documentation to allow the types to speak for themselves.
I would welcome your feedback on what I've done so far or
your suggestions for what to do next.  Please [contact
me](http://web.jaguarpaw.co.uk/~tom/contact/).

## Acknowledgements

Thanks to maintainer Michael Snoyman, and to all its other
contributors, for this great library!
