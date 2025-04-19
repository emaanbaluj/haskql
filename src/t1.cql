IMPORT "src/A1.csv" AS a;
PRINT a;

SET result AS (
    REPLACE a.COL(2) WITH a.COL(1)
);

PRINT result;