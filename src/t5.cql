IMPORT "src/A1.csv" AS a;
IMPORT "src/B1.csv" AS b;

SET c AS (
    LEFT_MERGE(a, b) ON COL 1
);

PRINT c;