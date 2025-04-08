IMPORT A.csv AS x;

SET y AS (
    CONCAT x AT x.COL(1) WITH {"foo", this}
);

PRINT y;