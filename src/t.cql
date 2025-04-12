IMPORT "A.csv" AS a;

SET b AS FILTER a.COL(3) == 4;

PRINT b;