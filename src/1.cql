
IMPORT A.csv AS a;
IMPORT B.csv AS b;

SET z AS (
    CROSS(a.$1, b) SORT(ASC) TRIM
);

PRINT z;