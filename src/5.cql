SET x AS EXTRACT A.csv
SET y AS EXTRACT B.csv

SET z AS (
    LEFT_MERGE x, y ON x.1 == y.1
)

PRINT y