.mod csv
.import /home/ouecon040/DScourseS20/FL_insurance_sample.csv datname

SELECT * FROM datname LIMIT 10;

SELECT DISTINCT county FROM datname;

SELECT AVG(tiv_2012 - tiv_2011) FROM datname;

SELECT construction, COUNT(*) FROM datname GROUP BY construction;

