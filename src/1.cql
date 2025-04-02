
IMPORT A.csv FROM "\User\user\haskql\csv";
IMPORT B.csv FROM "\User\user\haskql\csv";

SET z AS (
    SELECT * FROM c;
);

SET a AS (
    ...
);


SELECT * FROM z;

PRINT z;