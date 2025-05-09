IMPORT "R.csv" AS r;

SET z 
    AS CROSS(r,r,r) IN
    FILTER z WHERE COL(2) == COL(3) IN
    FILTER z WHERE COL(4) == COL(5) IN
    GET z.COL(1), z.COL(6);


PRINT z SORT;