# Python madness

```python
d = {}

for i in range(10):
  d[i] = lambda: i

for j in range(10):
  print(d[j]())


9
9
9
9
9
9
9
9
9
9
```

Not valid:

```python
(l for l in ls if l[2] == False for ls in lss)
```

Valid:

```
(l for ls in lss for l in ls if l[2] == False)
```

I guess the order of the `for`s mimcs the order in a nested for loop,
but then something like

```
(for ls in lss for l in ls if l[2] == False: l)
```

would be even closer.
