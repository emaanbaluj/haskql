IMPORT "A.csv" AS a;
IMPORT "B.csv" AS b;


SET c AS (
    CROSS(a, b)
);

PRINT c;