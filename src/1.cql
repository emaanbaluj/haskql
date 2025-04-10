IMPORT "src/A1.csv" AS a;
IMPORT "src/B1.csv" AS b;

SET c AS (
    CROSS(a, b)
);

PRINT a;