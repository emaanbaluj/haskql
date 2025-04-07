IMPORT "x/1/A.csv" AS x;

SET y AS (
    GET x.COL(3), x.COL(1) WHEN x.1 == x.2 SORT(ASC) TRIM
)

PRINT y

