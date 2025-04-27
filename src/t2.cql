IMPORT "src/A1.csv" AS x;

SET y AS (
    FILTER x.COL(1) == x.COL(2)
) IN (
    GET y.COL(3), y.COL(1)
);

PRINT y;

