IMPORT "src/A1.csv" AS a;

MAP (+10) IN a AS b;

PRINT b;

PRINT b.COL(1).ROW(1);