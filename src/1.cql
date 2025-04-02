IMPORT A.csv FROM "\User\user\haskql\csv"
IMPORT B.csv FROM "\User\user\haskql\csv"

SET x AS EXTRACT A.csv
SET y AS EXTRACT B.csv

SET z AS (
    CROSS(x, y) SORT ASC TRIM
)


PRINT z