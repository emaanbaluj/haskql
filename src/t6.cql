IMPORT "P.csv" AS p;
IMPORT "Q.csv" AS q;
IMPORT "R.csv" AS r;
IMPORT "S.csv" AS s;
IMPORT "T.csv" AS t;

SET z AS CROSS(p, q, r, s, t);

PRINT z;