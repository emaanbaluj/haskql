IMPORT "src/output.csv" AS students;

SET adults AS (
    FILTER students WHERE students.COL(2) >= 18
);

PRINT adults;

WRITE adults TO "src/adults.csv";