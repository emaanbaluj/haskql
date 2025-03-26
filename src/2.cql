SET x AS EXTRACT A.csv

SET y AS (
    GET x.3, x.1 WHEN x.1 == x.2 SORT ASC TRIM
)

PRINT y

