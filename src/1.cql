IMPORT "path/to/A.csv" AS a;

SET b AS (
    GET a.COL(1), a.COL(10)
);

PRINT b;