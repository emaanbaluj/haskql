IMPORT "src/output.csv" AS a;
MAP (+10) IN a AS b;
TRANSPOSE b AS c;
PRINT c;