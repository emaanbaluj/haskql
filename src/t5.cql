IMPORT "A.csv" AS a;
IMPORT "B.csv" AS b;

SET c AS (
    LEFT_MERGE(a, b) ON COL 1
);

PRINT c;