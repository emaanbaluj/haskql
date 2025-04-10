IMPORT "A.csv" AS x;

SET y AS (
    FILTER x.COL(2) IS NOT NULL
);

SET z AS (
    GET x.COL(1), x.COL(2)
);

PRINT z;

