IMPORT "A.csv" AS x;

SET y AS (
    CONCAT x.COL(1) WITH ("foo", $)
);

PRINT y;