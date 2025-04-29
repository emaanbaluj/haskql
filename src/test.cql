IMPORT "src/test1.csv" AS a;
IMPORT "src/test2.csv" AS b;

SET c AS (
    STACK(a, b)
);


PRINT c NOSORT;

