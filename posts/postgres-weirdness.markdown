# Postgres weirdness

* Array indices cannot be applied separately

````
select ('{{1,2}, {3,4}}' :: integer[])[1][2];
 int4 
------
    2
(1 row)

select ('{{1,2}, {3,4}}' :: integer[])[1];
 int4 
------
 
(1 row)

select foo[2]
from (select ('{{1,2}, {3,4}}' :: integer[])[1] as foo) as t1;
ERROR:  cannot subscript type integer because it is not an array

select ('{{1,2}, {3,4}}' :: integer[])[1:1][2:2];
 int4  
-------
 {{2}}
(1 row)

select foo[2:2]
from (select ('{{1,2}, {3,4}}' :: integer[])[1:1] as foo) as t1;
 foo 
-----
 {}
(1 row)
````

* `GROUP BY`

This orders by the second column

````
SELECT * from table ORDER BY 2;
````

whereas this orders by the constant 1 + 1, i.e. 2.

````
SELECT * from table ORDER BY 1 + 1;
````
