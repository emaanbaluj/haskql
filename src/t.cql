IMPORT "A.csv" AS a;

SET b AS FILTER a.COL(3) >= 1.1;

PRINT b;