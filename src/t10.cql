IMPORT "S.csv" AS s;
IMPORT "T.csv" AS t;

SET z AS 
    CROSS(s, t) IN
    FILTER z WHERE COL(1) == COL(2) IN
    FILTER z WHERE COL(5) == COL(6) IN
    GET z.COL(3), z.COL(4);

PRINT z;