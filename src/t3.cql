IMPORT "A.csv" AS x;

SET y AS (
    FILTER x WHERE COL(2) IS NOT NULL
) IN (
    GET y.COL(1), y.COL(2)
);

PRINT y;

