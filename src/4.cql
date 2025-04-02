IMPORT A.csv AS x;

SET y AS (
    REPLACE x.$1 WITH {x.1,"foo",x.1} SORT(ASC) TRIM
);

PRINT y;