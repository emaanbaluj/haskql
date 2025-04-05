
IMPORT A.csv AS a;
IMPORT B.csv AS b;

SET z (
    CROSS(a.COL(1), b) SORT(ASC) TRIM;
);

PRINT(z);