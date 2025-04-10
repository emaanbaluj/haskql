IMPORT "A.csv" AS a;
IMPORT "B.csv" AS b;

SET z AS (
    LEFT_MERGE(x, y) ON x.COL(1) == y.COL(1)
);

PRINT y;