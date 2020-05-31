# How I use `tmp-postgres`

```
PATH="/usr/lib/postgresql/11/bin/:${PATH}" v2-repl --build-depends tmp-postgres==1.34.1.0
ghci> import Database.Postgres.Temp
ghci> Right db <- start
ghci> toConnectionString db
"host=/tmp/tmp-postgres-socket-559194f2 dbname=postgres port=35013"
<do stuff with that information>
ghci> stop db
```

You have to make sure that `inidb` is on your `PATH` which it won't be
by default in Debian (nor Ubuntu, I think).
