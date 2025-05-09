IMPORT "P.csv" AS p;
IMPORT "Q.csv" AS q;

SET z AS RIGHT_MERGE(p,q) ON COL 4;

PRINT z;