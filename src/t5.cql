IMPORT "P.csv" AS a;
IMPORT "Q.csv" AS b;

SET c AS (
    LEFT_MERGE(a, b) ON COL 1
);

PRINT c;